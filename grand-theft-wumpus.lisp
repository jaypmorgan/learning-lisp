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
      (traverse-node))
    visited))

(defun find-islands (nodes edge-list)
  "Find disjoint graphs"
  (let ((islands nil))
    (labels ((find-islands (nodes)
	       ;; find the connected and unconnected nodes
	       ;; traverse the recursively traverse the unconnected nodes
	       ;; to find more islands
	       (let* ((connected (get-connected (car-nodes) edge-list))
		      (unconnected (set-difference nodes connected)))
		 (push connected islands)
		 (when unconncected
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
  (let* ((nodes (loop for i from 1 to *node-num* collect i))
	 (edge-list (connect-all-islands nodes (make-edge-list)))
	 (cops (remove-if-not (lambda (x) (zerop (random *cop-odds*))) edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))
