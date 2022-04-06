;;;=========================================================================
;;; massoc
;;;
;;; get multiple items from an association list (instead of just 1st one)
;;;
;;; From: Tony Tanzillo (tony.tanzillo@worldnet.att.net)
;;; Subject: Re: extracting multiple assoc from list
;;; Newsgroups: autodesk.autocad.customization
;;; Date: 1999/09/29
;;;
;;; revised by Dan, 2017
;;; to add option for key to be a list of assoc codes, '(10 11), or just a single
;;;=========================================================================
(defun massoc (key alist / x nlist)
  (if (/= 'LIST (type key))
      (setq key (list key)))
  (foreach y key
           (foreach x alist
                    (if (eq y (car x))
                        (setq nlist (cons x nlist)))))
  nlist)

;; c:bbb of cad-helper

(defun x:process-find-circle-meter (en / cc radius pt1 pt2 ss ed-line ed-line-half-len)
  (setq cc (cadr (assoc 10 (entget en))))
  (setq radius (cdr (assoc 40 (entget en))))
  (setq pt1 (- cc radius))
  (setq pt2 (+ cc radius))
  (setq pt1 (subst pt1 cc (assoc 10 (entget en))))
  (setq pt2 (subst pt2 cc (assoc 10 (entget en))))
  (setq pt2 (cons 11 (cdr pt2)))
  (setq ss (ssget "W" (cdr pt1) (cdr pt2) '((0 . "LINE"))))
  (if ss
      (progn
        (setq ed-line (entget (ssname ss 0)))
        (setq ed-line-half-len (abs (/ (- (cadr (assoc 10 ed-line)) (cadr (assoc 11 ed-line))) 2)))
        (if (equal (rtos ed-line-half-len) (rtos radius))
            (progn
              (setq __ss_nr (1+ __ss_nr))
              (print "find one meter, current nr: ")
              (princ __ss_nr)
              (if __ss
                  (setq __ss (ssadd en __ss))
                (setq __ss (ssadd en)))
              (ssadd (ssname ss 0) __ss))))))

(defun x:process-find-rectangle-meter (en / pt-lst pt1 pt2 x-len y-len ss ed-line ed-line-x-len ed-line-y-len)
  (setq ed (entget en))
  (setq pt-lst (massoc 10 ed))
  (if (= 4 (length pt-lst))
      (progn
        (setq pt1 (nth 0 pt-lst))
        (setq pt2 (nth 2 pt-lst))
        (setq x-len (abs (- (cadr pt1) (cadr pt2))))
        (setq y-len (abs (- (caddr pt1) (caddr pt2))))
        (setq ss (ssget "W" (cdr pt1) (cdr pt2) '((0 . "LINE"))))
        (if ss
            (progn
              (setq ed-line (entget (ssname ss 0)))
              (setq ed-line-x-len (abs (- (cadr (assoc 10 ed-line)) (cadr (assoc 11 ed-line)))))
              (setq ed-line-y-len (abs (- (caddr (assoc 10 ed-line)) (caddr (assoc 11 ed-line)))))
              (if (or (equal (rtos ed-line-x-len) (rtos x-len)) (equal (rtos ed-line-y-len) (rtos y-len)))
                  (progn
                    (setq __ss_nr (1+ __ss_nr))
                    (print "find one meter, current nr: ")
                    (princ __ss_nr)
                    (if __ss
                        (setq __ss (ssadd en __ss))
                      (setq __ss (ssadd en)))
                    (ssadd (ssname ss 0) __ss)))))))
  ())

(defun c:bbb (/ pt1 pt2 ss ssidx en)
  ; init __ss
  (setq __ss nil)
  (setq __ss_nr 0)

  ; get selected window
  (setq pt1 (getpoint "\nget point 1:"))
  (princ pt1)
  (setq pt2 (getpoint "\nget point 2:"))
  (princ pt2)

  ; find circle meter
  (setq ss (ssget "W" pt1 pt2 '((0 . "CIRCLE"))))
  (setq ssidx 0)
  (while (and ss (/= ssidx (sslength ss)))
    (x:process-find-circle-meter (ssname ss ssidx))
    (setq ssidx (1+ ssidx)))

  ; find rectangle meter
  (setq ss (ssget "W" pt1 pt2 '((0 . "LWPOLYLINE"))))
  (setq ssidx 0)
  (while (and ss (/= ssidx (sslength ss)))
    (x:process-find-rectangle-meter (ssname ss ssidx))
    (setq ssidx (1+ ssidx)))

  ; process result
  (princ "\n")
  (if __ss
      (command "change" __ss "" "properties" "color" "red" ""))
  (print "total nr: ")
  (princ __ss_nr)
  (princ))

(defun c:ball (/ ss ssidx en)
  ; init __ss
  (setq __ss nil)
  (setq __ss_nr 0)

  ; find circle meter
  (setq ss (ssget "X" '((0 . "CIRCLE"))))
  (setq ssidx 0)
  (while (and ss (/= ssidx (sslength ss)))
    (x:process-find-circle-meter (ssname ss ssidx))
    (setq ssidx (1+ ssidx)))

  ; process result
  (if __ss
      (command "change" __ss "" "properties" "color" "red" ""))
  (print "total nr: ")
  (princ __ss_nr)
  (princ))
