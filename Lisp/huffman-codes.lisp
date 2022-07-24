;;;; -*- Mode: Lisp -*-
;;;; begin of file: huffman-codes.lisp

;;;; 866169 Cristian Livella
;;;; Simple huffman encode library

;;; UTILS FUNCTIONS

;;; Return the lenght of a list.
(defun len (list)
  (cond
   ((null list) 0)
   (T (+ 1 (len (cdr list))))))

;;; Return a list of tree nodes (leaves)
;;; from a list of symbols and weights (symbol . weight).
(defun create-nodes (symbols-n-weights)
  (cond
   ((null symbols-n-weights) NIL)
   (T (append (list (cons
		     (cdr (car symbols-n-weights))
		     (list (car (car symbols-n-weights)))))
	      (create-nodes (cdr symbols-n-weights))))))

;;; Return the list of nodes sorted by weigts.
(defun sort-nodes (nodes)
  (stable-sort nodes (lambda (node1 node2)
		       (if (< (car node1) (car node2)) T NIL))))

;;; Return the list of nodes, with the first two grouped in a new node.
(defun group-first-two-nodes (nodes)
  (append (list (cons
		 (+ (car (first nodes)) (car (second nodes)))
		 (list (first nodes) (second nodes))))
	  (cdr (cdr nodes))))

;;; Return a list of symbols, containing the decoded message,
;;; given the encoded message and the symbol bits table.
(defun decode (bits symbol-bits-table &optional bits-head)
  ((lambda (symbol)
     (cond
      ((and (null bits) (not (null bits-head))) (error "Cannot find bits"))
      ((null bits) NIL)
      ((null symbol)
       (decode (cdr bits)
               symbol-bits-table
               (append bits-head (list (car bits)))))
      (T (append (list symbol) (decode (cdr bits) symbol-bits-table (list))))))
   (get-symbol-from-bits
    symbol-bits-table
    (append bits-head (list (car bits))))))

;;; Return a list of bits, containing the encoded message,
;;; given the original message (a list of symbol) and the symbol bits table.
(defun encode (message symbol-bits-table)
  (cond
   ((null message) NIL)
   (T (append (get-bits-from-symbol symbol-bits-table (car message))
	      (encode (cdr message) symbol-bits-table)))))

;;; Return the symbol bits table, given a huffman-tree.
(defun generate-symbol-bits-table (huffman-tree &optional prefix)
  (cond
   ((and (listp huffman-tree) (> (len huffman-tree) 1))
    (append
     (generate-symbol-bits-table
      (cdr(first huffman-tree))
      (append prefix '(0)))
     (generate-symbol-bits-table
      (cdr (second huffman-tree))
      (append prefix '(1)))))
   ((null prefix) (list (cons (car huffman-tree) (list 0))))
   (T (list (cons (car huffman-tree) prefix)))))

;;; Return a huffman tree from a list of nodes.
(defun generate-huffman-tree (nodes)
  (cond
   ((= (len nodes) 1) (car nodes))
   (T (generate-huffman-tree
       (group-first-two-nodes (sort-nodes nodes))))))

;;; Return a symbol given a symbol bits table and a list of bits to lookup.
(defun get-symbol-from-bits (symbol-bits-table bits)
  (cond
   ((null symbol-bits-table) NIL)
   ((equal (cdr (first symbol-bits-table)) bits)
    (car (first symbol-bits-table)))
   (T (get-symbol-from-bits (cdr symbol-bits-table) bits))))

;;; Return a list of bits given a symbol bits table and a symbol to lookup.
(defun get-bits-from-symbol (symbol-bits-table symbol)
  (cond
   ((null symbol-bits-table) (error "Cannot find symbol"))
   ((equal(car (first symbol-bits-table)) symbol)
    (cdr (first symbol-bits-table)))
   (T (get-bits-from-symbol (cdr symbol-bits-table) symbol))))

;;; Return a list of characters given a file stream.
(defun file-stream-to-list (stream)
  ((lambda (line)
     (cond
      ((not (null line))
       (append
	(coerce line 'list)
	(list #\Newline)
	(file-stream-to-list stream)))))
   (read-line stream nil)))

;;; Return a list of characters containing the choised number of spaces.
(defun get-spaces (count)
  (cond
   ((= count 0) NIL)
   (T (append (list #\Space) (get-spaces (- count 1))))))

;;; Return a list of characters than compose
;;; a graphical rappresentation of the given huffman tree.
(defun get-tree-for-print (huffman-tree &optional (indent-level 0))
  (cond
   ((null huffman-tree) nil)
   ((listp (car (cdr huffman-tree)))
    (append
     (get-spaces (* indent-level 4))
     (list #\( (car huffman-tree) #\) #\Newline)
     (get-tree-for-print (car (cdr huffman-tree)) (+ indent-level 1))
     (get-tree-for-print (car (cdr (cdr huffman-tree))) (+ indent-level 1))))
   ((null huffman-tree) nil)
   (T (append
       (get-spaces (* indent-level 4))
       (list
        #\(
        (car huffman-tree)
        #\)
        #\Space
	(car (cdr huffman-tree))
	#\Newline)))))

;;; API FUNCTIONS

;;; Return a list of symbols, containg the decoded message,
;;; given the encoded message and the huffman tree.
(defun he-decode (bits huffman-tree)
  (decode bits (he-generate-symbol-bits-table huffman-tree)))

;;; Return a list of bits, containing the encoded message,
;;; given the original message (a list of symbol) and the huffman tree.
(defun he-encode (message huffman-tree)
  (encode message (he-generate-symbol-bits-table huffman-tree)))

;;; Return a list of bits, containing the encoded file content,
;;; given a filename and the huffman tree.
(defun he-encode-file (filename huffman-tree)
  (with-open-file (stream filename)
		  (he-encode (file-stream-to-list stream) huffman-tree)))

;;; Return a huffman tree form a list of symbols-n-weights (symbol . weight).
(defun he-generate-huffman-tree (symbols-n-weights)
  (generate-huffman-tree (create-nodes symbols-n-weights)))

;;; Return the symbol bits table, given a huffman tree.
(defun he-generate-symbol-bits-table (huffman-tree)
  (generate-symbol-bits-table (cdr huffman-tree)))

;;; Print the given huffman tree.
(defun he-print-huffman-tree (huffman-tree &optional (indent-level 0))
  (format t "~&~{~A~}~%" (get-tree-for-print huffman-tree indent-level)))

;;; end of file -- huffman-codes.lisp
