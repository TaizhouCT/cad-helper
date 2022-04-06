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

(defun x:process-find-circle-meter (en / x-cc radius pt1 pt2 ss ed-line ed-line-half-len)
  (setq x-cc (cadr (assoc 10 (entget en))))
  (setq radius (cdr (assoc 40 (entget en))))
  (setq pt1 (- x-cc radius))
  (setq pt2 (+ x-cc radius))
  (setq pt1 (subst pt1 x-cc (assoc 10 (entget en))))
  (setq pt2 (subst pt2 x-cc (assoc 10 (entget en))))
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

(defun x:point-equal (pt1 pt2)
  (and (equal (rtos (car pt1)) (rtos (car pt2))) (equal (rtos (cadr pt1)) (rtos (cadr pt2)))))

(defun x:has-line (line ss ssidx / item pt1 pt2 result)
  (if (/= ssidx (sslength ss))
      (progn
        (setq item (entget (ssname ss ssidx)))
        (setq pt1 (cdr (assoc 10 item)))
        (setq pt2 (cdr (assoc 11 item)))
        (setq result nil)
        (if (or (and (x:point-equal (car line) pt1) (x:point-equal (cadr line) pt2))
                (and (x:point-equal (car line) pt2) (x:point-equal (cadr line) pt1)))
            (ssname ss ssidx)
          (x:has-line line ss (1+ ssidx))))
    nil))

(defun x:process-find-rectangle-meter (en / pt-lst pt1 pt2 x-center y-center horizon-center-line vertical-center-line ss ret)
  (setq pt-lst (massoc 10 (entget en)))
  (if (= 4 (length pt-lst))
      (progn
        (setq pt1 (nth 0 pt-lst))
        (setq pt2 (nth 2 pt-lst))
        (setq x-center (/ (+ (cadr pt1) (cadr pt2)) 2))
        (setq y-center (/ (+ (caddr pt1) (caddr pt2)) 2))
        (setq horizon-center-line (list (list x-center (caddr pt1)) (list x-center (caddr pt2))))
        (setq vertical-center-line (list (list (cadr pt1) y-center) (list (cadr pt2) y-center)))
        (setq ss (ssget "W" (cdr pt1) (cdr pt2) '((0 . "LINE"))))
        (if ss
            (progn
              (setq ret (x:has-line horizon-center-line ss 0))
              (if (not ret)
                  (setq ret (x:has-line vertical-center-line ss 0)))
              (if ret
                  (progn
                    (setq __ss_nr (1+ __ss_nr))
                    (print "find one meter, current nr: ")
                    (princ __ss_nr)
                    (if __ss
                        (setq __ss (ssadd en __ss))
                      (setq __ss (ssadd en)))
                    (ssadd ret __ss)))))))
  ())

(defun x:process-find-half-circle-meter (en / y-cc radius pt1 pt2 ss ed-line ed-line-half-len)
  (setq y-cc (caddr (assoc 10 (entget en))))
  (setq radius (cdr (assoc 40 (entget en))))
  (setq pt1 (- y-cc radius))
  (setq pt2 (+ y-cc radius))
  (setq pt1 (subst pt1 y-cc (assoc 10 (entget en))))
  (setq pt2 (subst pt2 y-cc (assoc 10 (entget en))))
  (setq pt2 (cons 11 (cdr pt2)))
  (setq ss (ssget "W" (cdr pt1) (cdr pt2) '((0 . "LINE"))))
  (if ss
      (progn
        (setq ed-line (entget (ssname ss 0)))
        (setq ed-line-half-len (abs (/ (- (caddr (assoc 10 ed-line)) (caddr (assoc 11 ed-line))) 2)))
        (if (equal (rtos ed-line-half-len) (rtos radius))
            (progn
              (setq __ss_nr (1+ __ss_nr))
              (print "find one meter, current nr: ")
              (princ __ss_nr)
              (if __ss
                  (setq __ss (ssadd en __ss))
                (setq __ss (ssadd en)))
              (ssadd (ssname ss 0) __ss))))))

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

  ; find half circle meter
  (setq ss (ssget "W" pt1 pt2 '((0 . "ARC"))))
  (setq ssidx 0)
  (while (and ss (/= ssidx (sslength ss)))
    (x:process-find-half-circle-meter (ssname ss ssidx))
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
