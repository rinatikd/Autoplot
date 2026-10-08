;;; =========================================================
;;; AutoPlot Pro v157.7 — компактный
;;; Поиск рамок: INSERT + LWPOLYLINE/POLYLINE + LINE
;;; Дедупликация: перекрытие BBox >= 80%
;;; Печать: строго по внешней рамке, 1:1, отдельные PDF
;;; Команды: AUTOPLOT, APFRAMES
;;; =========================================================
(princ "\n[D0] AutoPlot v157.7\n")
(vl-load-com)

(setq *log-path*  (strcat (getenv "USERPROFILE") "/Downloads/AutoPlot_log.txt"))
(setq *diag-path* (strcat (getenv "USERPROFILE") "/Downloads/AutoPlot_frames_diag.txt"))
(setq *log-buf* '() *diag-buf* '() *log-lvl* 3)

(defun ap-log (lvl msg / nm)
  (setq nm (nth lvl '("ERROR" "WARN " "INFO " "DEBUG" "TRACE")))
  (if (<= lvl *log-lvl*)
    (progn
      (setq line (strcat "[" nm "] " msg))
      (setq *log-buf* (cons line *log-buf*))
      (princ (strcat line "\n")))))

(defun ap-w (lvl msg) (ap-log lvl msg))
(defun ap-e (m) (ap-w 0 m))
(defun ap-i (m) (ap-w 2 m))
(defun ap-d (m) (ap-w 3 m))
(defun ap-t (m) (ap-w 4 m))

(defun ap-diag (line)
  (setq *diag-buf* (cons line *diag-buf*))
  (princ (strcat line "\n")))

(defun ap-save (path buf / f)
  (setq f (open path "w"))
  (if f
    (progn (foreach ln (reverse buf) (write-line ln f))
           (close f) (princ (strcat "\nСохранено: " path "\n")))))

(defun ap-try (label expr / r)
  (setq r (vl-catch-all-apply expr))
  (if (vl-catch-all-error-p r) (ap-e (strcat label ": " (vl-catch-all-error-message r))) r))

;;; ГОСТ-форматы
(setq *gost* '(("A4" 210.0 297.0) ("A3" 297.0 420.0) ("A2" 420.0 594.0)
               ("A1" 594.0 841.0) ("A0" 841.0 1189.0)))

(defun ap-fmt-full (s)
  (cond ((= s "A4") "ISO без полей A4 (210.00 x 297.00 мм)")
        ((= s "A3") "ISO без полей A3 (297.00 x 420.00 мм)")
        ((= s "A2") "ISO без полей A2 (420.00 x 594.00 мм)")
        ((= s "A1") "ISO без полей A1 (594.00 x 841.00 мм)")
        ((= s "A0") "ISO без полей A0 (841.00 x 1189.00 мм)")
        (t s)))

(defun ap-fmt-num (n)
  (if (numberp n)
    (if (< n 10) (strcat "0" (itoa n)) (itoa n))
    "01"))

;;; Формат по размеру с допуском ±10%
(defun ap-fmt-size (w h / mn mx tol res s nm sw sh)
  (setq mn (float (min w h)) mx (float (max w h)) tol 0.10 res nil)
  (foreach s *gost*
    (setq nm (car s) sw (cadr s) sh (caddr s))
    (if (and (null res)
             (< (abs (- mn sw)) (* tol sw))
             (< (abs (- mx sh)) (* tol sh)))
      (setq res nm)))
  res)

(defun ap-name-p (nm)
  (wcmatch (strcase nm) "*РАМК*,*KPSP*,*ГОСТ*,*FRAME*,*ШТАМП*,*ЛИСТ А*"))

;;; BBox
(defun ap-bbox (obj / mn mx r)
  (setq r (ap-try "bbox"
    '(lambda ()
       (vla-getboundingbox obj 'mn 'mx)
       (setq mn (vlax-safearray->list mn) mx (vlax-safearray->list mx))
       (if (and mn mx (numberp (car mn)) (numberp (car mx)))
         (list mn mx (abs (- (car mx) (car mn))) (abs (- (cadr mx) (cadr mn))))))))
  r)

(defun ap-eff (obj / nm e)
  (setq nm (vla-get-Name obj))
  (setq e (vl-catch-all-apply 'vla-get-EffectiveName (list obj)))
  (if (vl-catch-all-error-p e) nm (if e e nm)))

;;; Перекрытие BBox
(defun ap-ovl (a b r / ax1 ay1 ax2 ay2 bx1 by1 bx2 by2 ix1 iy1 ix2 iy2 ia am)
  (if (or (not (listp (nth 1 a))) (not (listp (nth 1 b)))) nil
    (progn
      (setq ax1 (car (nth 1 a)) ay1 (cadr (nth 1 a))
            ax2 (car (nth 2 a)) ay2 (cadr (nth 2 a))
            bx1 (car (nth 1 b)) by1 (cadr (nth 1 b))
            bx2 (car (nth 2 b)) by2 (cadr (nth 2 b))
            ix1 (max ax1 bx1) iy1 (max ay1 by1)
            ix2 (min ax2 bx2) iy2 (min ay2 by2))
      (if (or (<= ix2 ix1) (<= iy2 iy1)) nil
        (progn
          (setq ia (* (- ix2 ix1) (- iy2 iy1))
                am (min (* (- ax2 ax1) (- ay2 ay1))
                        (* (- bx2 bx1) (- by2 by1))))
          (if (<= am 0.0) nil (>= (/ ia am) r)))))))

;;; Собрать рамку из блока
(defun ap-from-insert (obj / bb w h fmt nm enm o)
  (setq bb (ap-bbox obj))
  (if (and bb (setq fmt (ap-fmt-size (caddr bb) (cadddr bb))))
    (progn
      (setq nm (vla-get-Name obj) enm (ap-eff obj))
      (if (or (ap-name-p nm) (ap-name-p enm))
        (progn
          (setq o (if (> (caddr bb) (cadddr bb)) "Альбомная" "Книжная"))
          (list (strcat "Рамка " fmt " [" o "]")
                (car bb) (cadr bb) fmt o (getvar "CTAB") 0))
        (progn
          (setq o (if (> (caddr bb) (cadddr bb)) "Альбомная" "Книжная"))
          (list (strcat "Рамка " fmt " [" o "]")
                (car bb) (cadr bb) fmt o (getvar "CTAB") 0))))
    nil))

;;; Собрать рамку из полилинии
(defun ap-from-poly (obj / bb w h fmt o)
  (setq bb (ap-bbox obj))
  (if (and bb (setq fmt (ap-fmt-size (caddr bb) (cadddr bb))))
    (progn
      (setq o (if (> (caddr bb) (cadddr bb)) "Альбомная" "Книжная"))
      (list (strcat "Полилиния " fmt " [" o "]")
            (car bb) (cadr bb) fmt o (getvar "CTAB") 0))
    nil))

;;; Уникальные с округлением
(defun ap-uniq (lst step / out v)
  (setq out '())
  (foreach v lst
    (setq v (* step (fix (/ v step))))
    (if (not (member v out)) (setq out (cons v out))))
  out)

;;; Проверка: 4 стороны прямоугольника
(defun ap-4sides (lns x1 x2 y1 y2 tol / ht hb hl hr a b)
  (setq ht nil hb nil hl nil hr nil)
  (foreach ln lns
    (setq a (car ln) b (cadr ln))
    (if (and (< (abs (- (cadr a) y2)) tol) (< (abs (- (cadr b) y2)) tol)
             (<= (- (min (car a) (car b)) x1) tol)
             (<= (- x2 (max (car a) (car b))) tol))
      (setq ht t))
    (if (and (< (abs (- (cadr a) y1)) tol) (< (abs (- (cadr b) y1)) tol)
             (<= (- (min (car a) (car b)) x1) tol)
             (<= (- x2 (max (car a) (car b))) tol))
      (setq hb t))
    (if (and (< (abs (- (car a) x1)) tol) (< (abs (- (car b) x1)) tol)
             (<= (- (min (cadr a) (cadr b)) y1) tol)
             (<= (- y2 (max (cadr a) (cadr b))) tol))
      (setq hl t))
    (if (and (< (abs (- (car a) x2)) tol) (< (abs (- (car b) x2)) tol)
             (<= (- (min (cadr a) (cadr b)) y1) tol)
             (<= (- y2 (max (cadr a) (cadr b))) tol))
      (setq hr t)))
  (and ht hb hl hr))

;;; Рамки из отрезков LINE
(defun ap-from-lines ( / ss i ent lns xs ys x1 x2 y1 y2 w h fmt o out)
  (setq ss (ssget "_X" (list (cons 410 (getvar "CTAB")) '(0 . "LINE"))))
  (setq lns '() xs '() ys '() out '())
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq a (cdr (assoc 10 (entget ent)))
              b (cdr (assoc 11 (entget ent))))
        (setq lns (cons (list a b) lns))
        (setq xs (cons (car a) (cons (car b) xs)))
        (setq ys (cons (cadr a) (cons (cadr b) ys)))
        (setq i (1+ i)))))
  (setq xs (ap-uniq xs 0.5) ys (ap-uniq ys 0.5))
  (ap-d (strcat "  LINE: " (itoa (length lns))
                ", X: " (itoa (length xs)) ", Y: " (itoa (length ys))))
  (foreach x1 xs (foreach x2 xs
    (if (> x2 x1)
      (progn
        (setq w (- x2 x1))
        (foreach y1 ys (foreach y2 ys
          (if (> y2 y1)
            (progn
              (setq h (- y2 y1))
              (setq fmt (ap-fmt-size w h))
              (if (and fmt (ap-4sides lns x1 x2 y1 y2 0.5))
                (progn
                  (setq o (if (> w h) "Альбомная" "Книжная"))
                  (setq out (cons (list (strcat "LINE " fmt " [" o "]")
                                        (list x1 y1) (list x2 y2)
                                        fmt o (getvar "CTAB") 0) out))))))))))))
  out)

;;; Собрать все рамки вкладки
(defun ap-collect ( / tab ss i ent obj fd res)
  (setq tab (getvar "CTAB") res '())
  ;; INSERT
  (setq ss (ssget "_X" (list (cons 410 tab) '(0 . "INSERT"))))
  (if ss
    (progn
      (ap-d (strcat "INSERT: " (itoa (sslength ss))))
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i) obj (vlax-ename->vla-object ent))
        (setq fd (ap-from-insert obj))
        (if fd (progn (setq res (cons fd res)) (ap-t (strcat "  + " (car fd)))))
        (setq i (1+ i)))))
  ;; POLYLINE
  (setq ss (ssget "_X" (list (cons 410 tab)
                             '(-4 . "<OR") '(0 . "LWPOLYLINE")
                             '(0 . "POLYLINE") '(-4 . "OR>"))))
  (if ss
    (progn
      (ap-d (strcat "Полилиний: " (itoa (sslength ss))))
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i) obj (vlax-ename->vla-object ent))
        (setq fd (ap-from-poly obj))
        (if fd (progn (setq res (cons fd res)) (ap-t (strcat "  + " (car fd)))))
        (setq i (1+ i)))))
  ;; LINE
  (setq res (append res (ap-from-lines)))
  res)

;;; Дедупликация
(defun ap-dedup ( / ins pols seen out fd)
  (setq ins '() pols '())
  (foreach fd *ap-frames*
    (if (wcmatch (car fd) "Рамка*") (setq ins (cons fd ins))
      (setq pols (cons fd pols))))
  (setq pols (vl-remove-if
    '(lambda (p) (vl-some '(lambda (i) (ap-ovl p i 0.80)) ins)) pols))
  (setq seen '())
  (setq pols (vl-remove-if
    '(lambda (p)
       (if (vl-some '(lambda (s) (ap-ovl p s 0.95)) seen) t
         (progn (setq seen (cons p seen)) nil))) pols))
  (setq out (append ins pols))
  (vl-sort out '(lambda (a b)
    (if (< (abs (- (cadr (cadr a)) (cadr (cadr b)))) 1.0)
      (< (car (cadr a)) (car (cadr b)))
      (> (cadr (cadr a)) (cadr (cadr b)))))))

;;; Найти все рамки на вкладке
(defun ap-find ( / res)
  (setq res (ap-collect))
  (setq *ap-frames* (ap-dedup))
  (ap-d (strcat "Итого рамок: " (itoa (length *ap-frames*))))
  *ap-frames*)

;;; Печать
(defun ap-plot (tab fmt orient pt1 pt2 pdf / ok ofd ocd)
  (if (findfile pdf) (vl-file-delete pdf))
  (setq ofd (getvar "FILEDIA") ocd (getvar "CMDDIA"))
  (setvar "FILEDIA" 0) (setvar "CMDDIA" 0)
  (ap-i (strcat "PLOT " tab " / " fmt " -> " pdf))
  (vl-cmdf "_.-PLOT" "Да" tab "DWG To PDF.pc3" fmt "Миллиметры" orient
           "Нет" "Рамка" pt1 pt2 "1:1" "Центрировать"
           "Да" "" "Нет" "Нет" "Нет" "Нет" pdf "Нет" "Да")
  (setvar "FILEDIA" ofd) (setvar "CMDDIA" ocd)
  (if (findfile pdf) (progn (ap-d (strcat "PDF: " pdf)) t)
    (progn (ap-e (strcat "Нет PDF: " pdf)) nil)))

(defun ap-print-frame (fd dir i / mn mx fmt o tab st pt1 pt2 pdf)
  (if (null fd) nil
    (progn
      (setq mn (cadr fd) mx (caddr fd)
            fmt (nth 3 fd) o (nth 4 fd) tab (nth 5 fd) st (nth 6 fd))
      (if (/= st 0) nil
        (progn
          (setq pt1 (strcat (rtos (car mn) 2 4) "," (rtos (cadr mn) 2 4))
                pt2 (strcat (rtos (car mx) 2 4) "," (rtos (cadr mx) 2 4))
                pdf (strcat dir "\\" (ap-fmt-num i) ".pdf"))
          (ap-plot tab (ap-fmt-full fmt) o pt1 pt2 pdf))))))

(defun ap-print-all ( / path dir pfx i fd ok err)
  (setq ok 0 err 0 i 0)
  (setq path (getfiled "Сохранить PDF" "" "pdf" 1))
  (if (null path) (princ "\nПуть не выбран.")
    (progn
      (setq dir (vl-filename-directory path)
            pfx (vl-filename-base (getvar "DWGNAME")))
      (if (= pfx "") (setq pfx "Drawing"))
      (setvar "BACKGROUNDPLOT" 0)
      (ap-i (strcat "=== ПЕЧАТЬ: " (itoa (length *ap-frames*)) " ==="))
      (foreach fd *ap-frames*
        (setq i (1+ i))
        (if (= (nth 6 fd) 1) (ap-d (strcat "[" (itoa i) "] пропуск"))
          (progn
            (princ (strcat "[" (itoa i) "] "))
            (if (ap-print-frame fd dir i)
              (progn (ap-i (strcat "[" (itoa i) "] OK"))
                     (princ "OK\n") (setq ok (1+ ok)))
              (progn (ap-e (strcat "[" (itoa i) "] ERR"))
                     (princ "ERR\n") (setq err (1+ err)))))))
      (ap-i (strcat "=== ИТОГ: OK=" (itoa ok) " ERR=" (itoa err) " ==="))
      (princ (strcat "\n=== ИТОГ: OK=" (itoa ok) " ERR=" (itoa err) " ===\n"))
      (ap-save *log-path* *log-buf*))))

;;; ---------- AUTOPLOT ----------
(defun c:AUTOPLOT ( / )
  (vl-load-com)
  (setq *log-buf* '() *log-lvl* 4)
  (ap-i "========================================")
  (ap-i (strcat "AutoPlot v157.7 — "
                (menucmd "M=$(edtime,$(getvar,date),DD.MM.YYYY HH:MM:SS)")))
  (ap-i "========================================")
  (ap-find)
  (if (= (length *ap-frames*) 0)
    (progn (ap-w 1 "Рамки не найдены.") (princ "\nРамки не найдены.\n"))
    (ap-print-all))
  (princ))

;;; ---------- APFRAMES ----------
(defun c:APFRAMES ( / tab ss i ent obj nm enm bb w h fmt ok rej)
  (setq *diag-buf* '())
  (ap-diag "========================================")
  (ap-diag " APFRAMES v157.7")
  (ap-diag (strcat " " (menucmd "M=$(edtime,$(getvar,date),DD.MM.YYYY HH:MM:SS)")))
  (ap-diag "========================================")
  (setq tab (getvar "CTAB"))
  (ap-diag (strcat "Вкладка: " tab
                   " TILEMODE=" (itoa (getvar "TILEMODE"))))
  (setq ss (ssget "_X" (list (cons 410 tab) '(0 . "INSERT"))))
  (ap-diag (strcat "INSERT: " (if ss (itoa (sslength ss)) "0")))
  (setq ok 0 rej 0)
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i) obj (vlax-ename->vla-object ent))
        (setq nm (vla-get-Name obj) enm (ap-eff obj) bb (ap-bbox obj))
        (if bb
          (progn
            (setq w (caddr bb) h (cadddr bb) fmt (ap-fmt-size w h))
            (ap-diag (strcat "  [" (itoa (1+ i)) "] '" nm "'"
                             (if (and enm (/= enm nm)) (strcat " eff='" enm "'") "")
                             " w=" (rtos w 2 4) " h=" (rtos h 2 4)
                             " fmt=" (if fmt fmt "nil")))
            (if fmt (setq ok (1+ ok)) (setq rej (1+ rej)))))
        (setq i (1+ i)))))
  (ap-diag (strcat "Блоков-рамок: " (itoa ok)
                   " отклонено: " (itoa rej)))
  (setq ss (ssget "_X" (list (cons 410 tab)
                             '(-4 . "<OR") '(0 . "LWPOLYLINE")
                             '(0 . "POLYLINE") '(-4 . "OR>"))))
  (ap-diag (strcat "Полилиний: " (if ss (itoa (sslength ss)) "0")))
  (ap-diag "--- РЕЗУЛЬТАТ ap-find ---")
  (ap-find)
  (ap-diag (strcat "Найдено рамок: " (itoa (length *ap-frames*))))
  (foreach fd *ap-frames*
    (ap-diag (strcat "  " (car fd))))
  (ap-save *diag-path* *diag-buf*)
  (princ "\n=== APFRAMES готов ===\n")
  (princ))

(princ "\n========================================\n")
(princ "  AutoPlot Pro v157.7 — загружен\n")
(princ (strcat "  Лог: " *log-path* "\n"))
(princ "  Команды: AUTOPLOT, APFRAMES\n")
(princ "========================================\n")
(princ)
