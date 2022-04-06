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
  (if (not (= 'LIST (type key)))
      (setq key (list key)))
  (foreach y key
           (foreach x alist
                    (if (eq y (car x))
                        (setq nlist (cons (cdr x) nlist))))))

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
              (print "find a meter")
              (if __ss
                  (setq __ss (ssadd en __ss))
                (setq __ss (ssadd en)))
              (ssadd (ssname ss 0) __ss))))))

(defun c:bbbp (/ pt1 pt2 ss ssidx en)
  ; init __ss
  (setq __ss nil)

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

  ; process result
  (if __ss
      (command "change" __ss "" "properties" "color" "red" ""))
  (print "number of meter: ")
  (if __ss
      (princ (/ (sslength __ss) 2))
    (princ 0))
  (princ))

(defun c:bbb (/ ss ssidx en)
  ; init __ss
  (setq __ss nil)

  ; find circle meter
  (setq ss (ssget "X" '((0 . "CIRCLE"))))
  (setq ssidx 0)
  (while (and ss (/= ssidx (sslength ss)))
    (x:process-find-circle-meter (ssname ss ssidx))
    (setq ssidx (1+ ssidx)))

  ; process result
  (if __ss
      (command "change" __ss "" "properties" "color" "red" ""))
  (print "number of meter: ")
  (if __ss
      (princ (/ (sslength __ss) 2))
    (princ 0))
  (princ))
