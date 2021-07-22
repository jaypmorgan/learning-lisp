;; Grand theft wumpus Game
;; 
;; Hunt down your back-stabbing partner in crime: the wumpus. He's
;; fled with the money to somewhere in congestion city. You will need
;; to track him down while avoiding the police and the glowworm gang.

;; Defining the edges in congestion city. This city will be an
;; undirected graph, where each node will contain the presence of the
;; wumpus, glowworm gang, and various danger signs.
(load "graph-util") ;; graphing functions created in the last chapter

;; set up the initial state of congestion city
(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *player-pos* nil)

;; global parameters of the game settings
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

(defun random-node ()
  "Return a random node identifier"
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  "Create two directed/undirected edge between two nodes"
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  "Generate a list of random edges"
  (apply #'append
	 (loop repeat *edge-num* collect
	       (edge-pair (random-node) (random-node)))))

(defun hash-edges (edge-list)
  (let ((tab (make-hash-table)))
    (mapc (lambda (x) (let ((node (car x)))
			(push (cdr x) (gethash node tab))))
	  edge-list)
    tab))

;; the above process, due to the randomness, can create islands so we
;; do the following to connect these islands to the rest of the city.
(defun direct-edges (node edge-list)
  "Finds all edges in an edge list that start from a given node. Create
  a new list with all edges removed that doesn't have the current node
  in the car position."
  (remove-if-not (lambda (x) (eql (car x) node)) edge-list))

(defun get-connected (node edge-list)
  "Build a list of all nodes connected to `node` even if you need to
  walk across multiple paths to get there"
  (let ((visited nil))
    (labels ((traverse (node)
	       (unless (member node visited)
		 (push node visited)
		 (mapc (lambda (edge) (traverse (cdr edge)))
		       (direct-edges node edge-list)))))
      (traverse node))
    visited))

(defun get-connected-hash (node edge-tab)
  (let ((visited (make-hash-table)))
    (labels ((traverse (node)
	       (unless (gethash node visited)
		 (setf (gethash node visited) t)
		 (mapc (lambda (edge) (traverse edge)) (gethash node edge-tab)))))
      (traverse node))
    visited))

(defun find-islands (nodes edge-list)
  "Find disjoint graphs"
  (let ((islands nil))
    (labels ((find-islands (nodes)
	       ;; find the connected and unconnected nodes
	       ;; traverse the recursively traverse the unconnected nodes
	       ;; to find more islands
	       (let* ((connected (get-connected (car nodes) edge-list))
		      (unconnected (set-difference nodes connected)))
		 (push connected islands)
		 (when unconnected
		   (find-islands unconnected)))))
      (find-islands nodes))
    islands))

(defun connect-with-bridges (islands)
  "Returns a list with a fully connected graph"
  (when (cdr islands) ;; when there is more than one island
    (append (edge-pair (caar islands) (caadr islands)) ;; connected the nodes of different islands
	    ;; recursive into the rest of the islands
	    (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  "Find all islands from the node and edge list and connect them,
returning a single graph"
  (append (connect-with-bridges (find-islands nodes edge-list))
	  edge-list))

(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
	    (cons node1 (mapcar (lambda (edge) (list (cdr edge)))
				(remove-duplicates (direct-edges node1 edge-list)
						   :test #'equal))))
	  (remove-duplicates (mapcar #'car edge-list))))

(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
	    (let ((node1 (car x))
		  (node1-edges (cdr x)))
	      (cons node1 (mapcar (lambda (edge)
				    (let ((node2 (car edge)))
				      (if (intersection (edge-pair node1 node2) edges-with-cops :test #'equal)
					  (list node2 'cops)
					  edge)))
				  node1-edges))))
	  edge-alist))

(defun make-city-edges ()
  "Create a list of nodes using a loop"
  (let* ((nodes (loop for i from 1 to *node-num* collect i))
	 (edge-list (connect-all-islands nodes (make-edge-list)))
	 (cops (remove-if-not (lambda (x) (declare (ignore x)) (zerop (random *cop-odds*))) edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

(defun neighbours (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

(defun within-one (a b edge-alist)
  (member b (neighbours a edge-alist)))

(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      (some (lambda (x)
	      (within-one x b edge-alist))
	    (neighbours a edge-alist))))

(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
	(glow-worms (loop for i below *worm-num* collect (random-node))))
    (loop for n from 1 to *node-num*
       collect (append (list n)
		       (cond ((eql n wumpus) '(wumpus))
			     ((within-two n wumpus edge-alist) '(blood!)))
		       (cond ((member n glow-worms) '(glow-worm))
			     ((some (lambda (worm)
				      (within-one n worm edge-alist))
				    glow-worms)
			      '(lights!)))
		       (when (some #'cdr (cdr (assoc n edge-alist)))
			 '(sirens!))))))

(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
	(find-empty-node)
	x)))

(defun draw-city ()
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))

(defun known-city-nodes ()
  (mapcar (lambda (node)
	    (if (member node *visited-nodes*)
		(let ((n (assoc node *congestion-city-nodes*)))
		  (if (eql node *player-pos*)
		      (append n '(*)) n))
		(list node '?)))
	  (remove-duplicates (append *visited-nodes* (mapcan (lambda (node)
							       (mapcar #'car (cdr (assoc node *congestion-city-edges*))))
							     *visited-nodes*)))))

(defun known-city-edges ()
  (mapcar (lambda (node)
	    (cons node (mapcar (lambda (x)
				 (if (member (car x) *visited-nodes*)
				     x
				     (list (car x))))
			       (cdr (assoc node *congestion-city-edges*)))))
	  *visited-nodes*))

(defun draw-known-city ()
  (ugraph->png "known-city" (known-city-nodes) (known-city-edges)))

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))

(defun walk (pos)
  (handle-direction pos nil))

(defun charge (pos)
  (handle-direction pos t))

(defun handle-direction (pos charging)
  (let ((edge (assoc pos (cdr (assoc *player-pos* *congestion-city-edges*)))))
    (if edge
	(handle-new-place edge pos charging)
	(princ "That location does not exist!"))))

(defun handle-new-place (edge pos charging)
  (let* ((node (assoc pos *congestion-city-nodes*))
	 (has-worm (and (member 'glow-worm node)
			(not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (draw-known-city)
    (cond ((member 'cops edge) (princ "You ran into the cops. Game Over."))
	  ((member 'wumpus node) (if charging (princ "You found the Wumpus!") (princ "You ran into the Wumpus")))
	  (charging (princ "You wasted your last bullet. Game Over."))
	  (has-worm (let ((new-pos (random-node)))
		      (princ "You ran into a Glow Worm Gang! You're not at a ")
		      (princ new-pos)
		      (handle-new-place nil new-pos nil))))))
