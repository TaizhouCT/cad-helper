(defun x:split (str delim / ptr lst)
  (while (setq ptr (vl-string-search delim str))
    (setq lst (cons (substr str 1 ptr) lst))
    (setq str (substr str (+ ptr 2))))
  (reverse (cons str lst)))

(defun c:ttt (/ fname fp line)
  (setq fname (getfiled "" "" "txt" 8))
  (if fname
    (progn
      (setq fp (open fname "r"))
      (setq line (read-line fp))
      (while line
        (print (x:split line "\t"))
        (setq line (read-line fp)))
      (close fp))
    (progn
      (print "not specifiy filename, exit ...")))
  (princ))
