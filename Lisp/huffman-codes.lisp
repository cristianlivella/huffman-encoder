(defun len (list)
  (cond
   ((null list) 0)
   (T (+ 1 (len (cdr list))))))

(defun create-nodes (pairs)
  (cond
    ((null pairs) NIL)
    (T (append (list (cons (cdr (car pairs)) (list (car (car pairs))))) (create-nodes (cdr pairs))))
    ))

(defun sort-nodes (nodes)
  (stable-sort nodes (lambda (node1 node2)
		       (if (< (car node1) (car node2)) T NIL))))

(defun real-generate-huffman (nodes)
  (append (list (cons
		 (+ (car (first nodes)) (car (second nodes)))
		 (list (first nodes) (second nodes))))
	  (cdr (cdr nodes))))
	 

;(defun real-generate-huffman (nodes)
;  ((lambda (sum-weight first-node second-node)
;     (cons (
;	    (+
;
;	   ) (list first-node second-node)))
;   ((+ (car (first nodes)) (car (second nodes))) (first nodes) (second nodes))))

(defun real-generate-symbol-bits-table (huffman-tree prefix)
  (cond
   ((and (listp huffman-tree) (> (len huffman-tree) 1)) (append
			  (real-generate-symbol-bits-table (cdr (first huffman-tree)) (append prefix '(0)))
			  (real-generate-symbol-bits-table (cdr (second huffman-tree)) (append prefix '(1)))))
   (T (list (cons huffman-tree prefix)))))
   

(defun he-generate-huffman-tree (nodes)
  (cond
   ((= (len nodes) 1) (car nodes))
   (T (final-generate-huffman (real-generate-huffman (sort-nodes nodes))))))

(defun get-symbol-from-bits (bits-table bits)
  (cond
   ((null bits-table) NIL)
   ((equal (cdr (first bits-table)) bits) (car (car (first bits-table))))
   (T (get-symbol-from-bits (cdr bits-table) bits))))

(defun get-bits-from-symbol (bits-table symbol)
  (cond
   ((null bits-table) NIL)
   ((equal (car (car (first bits-table))) symbol) (cdr (first bits-table)))
   (T (get-bits-from-symbol (cdr bits-table) symbol))))

(defun encode-real (message symbol-bits-table)
  (cond
   ((null message) NIL)
   (T (append (get-bits-from-symbol symbol-bits-table (car message)) (encode-real(cdr message) symbol-bits-table)))))


(defun decode-real (bits bits-head symbol-bits-table)
  ((lambda (symbol bits bits-head symbol-bits-table)
     (cond
      ((and (write bits) (write bits-head) (write symbol) (write "  ") NIL) 0)
      ((null bits) NIL)
      ((null symbol) (decode-real (cdr bits) (append bits-head (list (car bits))) symbol-bits-table))
      (T (append (list symbol) (decode-real (cdr bits) (list) symbol-bits-table)))))
   (get-symbol-from-bits symbol-bits-table (append bits-head (list (car bits)))) bits bits-head symbol-bits-table))

(defun file-stream-to-list (stream)
  ((lambda (line)
     (cond
      ((not (null line)) (append (coerce line 'list) (list #\Newline) (file-stream-to-list stream)))))
   (read-line stream nil)))

(defun get-file (filename)
  (with-open-file (stream filename)
    (file-stream-to-list stream)))

;(defun he-encode-file (filename huffman-tree)
;(defun he-generate-symbol-bits-table (huffman-tree)
;  (

(defun test (list)
  (cond ((null list) 0)
	(t (+ 1 (test (cdr list))))))
