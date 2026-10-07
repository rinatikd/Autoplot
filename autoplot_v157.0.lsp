;;; =========================================================
;;; AutoPlot Pro v157.0 — печать активной вкладки в отдельные PDF
;;; + самоопределение масштаба + самодиагностика + txt-лог
;;; Команды: AUTOPLOT, APFRAMES, APDIAG, APLOG, APDEBUG
;;; =========================================================
(princ "\n[D0] Старт v157.0\n")
(vl-load-com)
(princ "[D1] vl-load-com OK\n")

;;; ---------- Пути логов ----------
(setq *log-path*
      (strcat (getenv "USERPROFILE")
              "/Downloads/AutoPlot_log.txt"))
(setq *diag-path*
      (strcat (getenv "USERPROFILE")
              "/Downloads/AutoPlot_frames_diag.txt"))
(setq *log-buffer* '())
(setq *diag-buffer* '())
(setq *log-level* 3)
(setq *log-counts* '(0 0 0 0 0))
(setq *log-errors* '())
(setq *ap-scale* 1.0)
(princ "[D2] log-path OK\n")

;;; ---------- Лог ----------
(defun ap-level-name (lvl)
  (cond ((= lvl 0) "ERROR")
        ((= lvl 1) "WARN ")
        ((= lvl 2) "INFO ")
        ((= lvl 3) "DEBUG")
        ((= lvl 4) "TRACE")
        (t "     ")))

(defun ap-list-set (lst idx val / i out)
  (setq i 0 out '())
  (foreach x lst
    (setq out (cons (if (= i idx) val x) out))
    (setq i (1+ i)))
  (reverse out))

(defun ap-count-inc (lvl / c)
  (setq c (nth lvl *log-counts*))
  (setq *log-counts* (ap-list-set *log-counts* lvl (1+ c))))

(defun ap-write-level (lvl msg)
  (ap-count-inc lvl)
  (if (<= lvl *log-level*)
    (progn
      (setq line (strcat "[" (ap-level-name lvl) "] " msg))
      (setq *log-buffer* (cons line *log-buffer*))
      (princ (strcat line "\n")))))

(defun ap-err  (msg) (ap-write-level 0 msg) (setq *log-errors* (cons msg *log-errors*)))
(defun ap-warn (msg) (ap-write-level 1 msg))
(defun ap-info (msg) (ap-write-level 2 msg))
(defun ap-dbg  (msg) (ap-write-level 3 msg))
(defun ap-trc  (msg) (ap-write-level 4 msg))

(defun ap-diag (line)
  (setq *diag-buffer* (cons line *diag-buffer*))
  (princ (strcat line "\n")))

(defun ap-save-log ( / f)
  (setq f (open *log-path* "w"))
  (if f
    (progn
      (write-line "========================================" f)
      (write-line " AutoPlot log" f)
      (write-line "========================================" f)
      (foreach line (reverse *log-buffer*)
        (write-line line f))
      (write-line "----------------------------------------" f)
      (write-line (strcat "ERROR: " (itoa (nth 0 *log-counts*))) f)
      (write-line (strcat "WARN : " (itoa (nth 1 *log-counts*))) f)
      (write-line (strcat "INFO : " (itoa (nth 2 *log-counts*))) f)
      (write-line (strcat "DEBUG: " (itoa (nth 3 *log-counts*))) f)
      (write-line (strcat "TRACE: " (itoa (nth 4 *log-counts*))) f)
      (close f)
      (princ (strcat "\nЛог сохранён: " *log-path* "\n")))
    (princ "\nНе удалось открыть файл лога.\n")))

(defun ap-save-diag ( / f)
  (setq f (open *diag-path* "w"))
  (if f
    (progn
      (foreach line (reverse *diag-buffer*)
        (write-line line f))
      (close f)
      (princ (strcat "\nОтчёт диагностики: " *diag-path* "\n")))
    (princ "\nНе удалось открыть файл диагностики.\n")))
(princ "[D3] ap-write OK\n")

(defun ap-try (label expr / res)
  (setq res (vl-catch-all-apply expr))
  (if (vl-catch-all-error-p res)
    (progn (ap-err (strcat label ": " (vl-catch-all-error-message res))) nil)
    res))
(princ "[D4] ap-try OK\n")

(defun ap-send (cmd) (vl-cmdf cmd))
(princ "[D5] ap-send OK\n")

;;; ---------- ГОСТ-форматы ----------
(setq *gost-formats*
      '(("A4" 210.0 297.0)
        ("A3" 297.0 420.0)
        ("A2" 420.0 594.0)
        ("A1" 594.0 841.0)
        ("A0" 841.0 1189.0)))

(defun ap-format-full (short)
  (cond
    ((= short "A4") "ISO без полей A4 (210.00 x 297.00 мм)")
    ((= short "A3") "ISO без полей A3 (297.00 x 420.00 мм)")
    ((= short "A2") "ISO без полей A2 (420.00 x 594.00 мм)")
    ((= short "A1") "ISO без полей A1 (594.00 x 841.00 мм)")
    ((= short "A0") "ISO без полей A0 (841.00 x 1189.00 мм)")
    (t short)))

(defun ap-format-num (n)
  (if (numberp n)
    (if (< n 10) (strcat "0" (itoa n)) (itoa n))
    "01"))
(princ "[D6] formats OK\n")

;;; «Красивый» масштаб (1, 2, 5 × 10^n)
(defun ap-nice-scale-p (k / v)
  (if (<= k 0.0) nil
    (progn
      (setq v k)
      (while (< v 1.0) (setq v (* v 10.0)))
      (while (>= v 10.0) (setq v (/ v 10.0)))
      (or (< (abs (- v 1.0)) 0.01)
          (< (abs (- v 2.0)) 0.02)
          (< (abs (- v 5.0)) 0.05)))))

;;; Определить формат и k для одиночной рамки
(defun ap-guess-format-scale (w h / mn mx best bestk bestfmt nm sw sh k)
  (setq mn (float (min w h)) mx (float (max w h)))
  (setq best nil bestk 1.0 bestfmt nil)
  (foreach s *gost-formats*
    (setq nm (car s) sw (cadr s) sh (caddr s))
    (setq k (/ mn sw))
    (if (and (ap-nice-scale-p k)
             (< (abs (- (/ mx (max 0.0001 mn)) (/ sh sw))) 0.06))
      (if (or (null best)
              (< (abs (- (log k) 0.0)) (abs (- (log bestk) 0.0))))
        (progn (setq best (cons nm k) bestk k bestfmt nm)))))
  best)

;;; Определить масштаб по первой подходящей рамке из списка
(defun ap-detect-first-pair (bb-list / res bb w h r)
  (setq res nil)
  (foreach bb bb-list
    (if (null res)
      (progn
        (setq w (caddr bb) h (cadddr bb))
        (setq r (ap-guess-format-scale w h))
        (if r (setq res r)))))
  res)

;;; Проверить соответствие ГОСТ-формату при заданном масштабе
(defun ap-format-by-scale (w h k / mn mx tol result nm sw sh)
  (setq mn (float (min w h)) mx (float (max w h)))
  (setq tol 0.10)
  (setq result nil)
  (foreach s *gost-formats*
    (setq nm (car s) sw (cadr s) sh (caddr s))
    (if (and (null result)
             (< (abs (- mn (* sw k))) (* tol sw k))
             (< (abs (- mx (* sh k))) (* tol sh k)))
      (setq result nm)))
  result)
(princ "[D7] scale-detect OK\n")

;;; ---------- BBox / полилинии ----------
(defun ap-get-bbox (obj / mn mx result)
  (setq result
    (ap-try "ap-get-bbox"
      '(lambda ()
         (vla-getboundingbox obj 'mn 'mx)
         (setq mn (vlax-safearray->list mn)
               mx (vlax-safearray->list mx))
         (if (and mn mx (numberp (car mn)) (numberp (car mx)))
           (list mn mx
                 (abs (- (car mx) (car mn)))
                 (abs (- (cadr mx) (cadr mn))))))))
  result)

(defun ap-is-closed-true (ent)
  (let ((c (vl-catch-all-apply 'vla-get-Closed (list ent))))
    (if (vl-catch-all-error-p c)
      nil
      (or (eq c :vlax-true)
          (eq c 1)
          (eq c T)
          (and (numberp c) (/= c 0))))))

(defun ap-count-closed (blk_name depth / ad br ent on cnt)
  (setq cnt 0)
  (if (<= depth 4)
    (ap-try "ap-count-closed"
      '(lambda ()
         (setq ad (vla-get-ActiveDocument (vlax-get-acad-object)))
         (setq br (vla-item (vla-get-Blocks ad) blk_name))
         (vlax-for ent br
           (setq on (vla-get-ObjectName ent))
           (cond
             ((wcmatch on "AcDb*Polyline")
              (if (ap-is-closed-true ent)
                (setq cnt (1+ cnt))))
             ((= on "AcDbBlockReference")
              (setq cnt (+ cnt (ap-count-closed (vla-get-Name ent)
                                                (1+ depth))))))))))
  cnt)

(defun ap-reject-reason (fmt closed w h k)
  (cond
    ((null fmt)
     (strcat "формат не распознан (w=" (rtos w 2 4)
             " h=" (rtos h 2 4)
             " k=" (rtos k 2 6)
             " ratio=" (rtos (/ (max w h) (max 0.0001 (min w h))) 2 3) ")"))
    ((< closed 2)
     (strcat "замкнутых полилиний < 2 (closed=" (itoa closed) ")"))
    (t "неизвестная причина")))
(princ "[D8] helpers OK\n")

;;; ---------- Модель ----------
(defun ap-find-model ( / tab ss i ent obj bb w h fmt orient closed bb-list scale)
  (setq tab (getvar "CTAB"))
  (ap-dbg (strcat "ap-find-model: tab=" tab))
  (setq ss (ssget "_X" (list (cons 410 tab) '(0 . "INSERT"))))
  (setq bb-list '())
  (if ss
    (progn
      (ap-dbg (strcat "  INSERT найдено: " (itoa (sslength ss))))
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i) obj (vlax-ename->vla-object ent))
        (setq closed (ap-count-closed (vla-get-Name obj) 0))
        (setq bb (ap-get-bbox obj))
        (if (and bb (>= closed 2))
          (setq bb-list (cons bb bb-list)))
        (setq i (1+ i)))))
  (setq scale (ap-detect-first-pair bb-list))
  (if scale
    (progn
      (setq *ap-scale* (cdr scale))
      (ap-info (strcat "Масштаб определён: 1:"
                       (rtos (/ 1.0 (cdr scale)) 2 3)
                       " (k=" (rtos (cdr scale) 2 6)
                       ", fmt=" (car scale) ")")))
    (progn
      (setq *ap-scale* 1.0)
      (ap-warn "Масштаб не определён — использую 1.0")))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i) obj (vlax-ename->vla-object ent))
        (setq closed (ap-count-closed (vla-get-Name obj) 0))
        (setq bb (ap-get-bbox obj))
        (if (and bb (>= closed 2))
          (progn
            (setq w (caddr bb) h (cadddr bb))
            (setq fmt (ap-format-by-scale w h *ap-scale*))
            (ap-trc (strcat "  INSERT '" (vla-get-Name obj)
                            "' w=" (rtos w 2 4)
                            " h=" (rtos h 2 4)
                            " closed=" (itoa closed)
                            " fmt=" (if fmt fmt "nil")))
            (if fmt
              (progn
                (setq orient (if (> w h) "Альбомная" "Книжная"))
                (setq *ap-frame-data*
                      (cons (list (strcat "Модель: " fmt " [" orient "]")
                                  (car bb) (cadr bb)
                                  fmt orient "Model" 0)
                            *ap-frame-data*))
                (setq *ap-frames* (cons fmt *ap-frames*)))
              (ap-dbg (strcat "  Отклонена: "
                              (ap-reject-reason fmt closed w h *ap-scale*))))))
        (setq i (1+ i))))))
(princ "[D9] ap-find-model OK\n")

;;; ---------- Лист ----------
(defun ap-find-current ( / tab ss i ent obj on bb w h fmt orient
                          closed cnt total rejected bb-list scale)
  (setq tab (getvar "CTAB"))
  (ap-dbg (strcat "ap-find-current: tab=" tab))
  (setq ss (ssget "_X" (list (cons 410 tab)
                             '(-4 . "<OR")
                             '(0 . "LWPOLYLINE")
                             '(0 . "POLYLINE")
                             '(0 . "INSERT")
                             '(-4 . "OR>"))))
  (setq cnt 0 total 0 rejected 0 bb-list '())
  (if ss
    (progn
      (ap-dbg (strcat "  Объектов на листе: " (itoa (sslength ss))))
      ;; 1-й проход: собрать BBox подходящих рамок
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i)
              obj (vlax-ename->vla-object ent)
              on (vl-catch-all-apply 'vla-get-ObjectName (list obj)))
        (if (vl-catch-all-error-p on) (setq on "?"))
        (cond
          ((or (= on "AcDbPolyline") (= on "AcDb2dPolyline"))
           (setq closed (ap-is-closed-true obj))
           (setq bb (ap-get-bbox obj))
           (if (and bb closed)
             (setq bb-list (cons bb bb-list))))
          ((= on "AcDbBlockReference")
           (setq closed (ap-count-closed (vla-get-Name obj) 0))
           (setq bb (ap-get-bbox obj))
           (if (and bb (>= closed 2))
             (setq bb-list (cons bb bb-list)))))
        (setq i (1+ i)))
      ;; Определить масштаб
      (setq scale (ap-detect-first-pair bb-list))
      (if scale
        (progn
          (setq *ap-scale* (cdr scale))
          (ap-info (strcat "Масштаб определён: 1:"
                           (rtos (/ 1.0 (cdr scale)) 2 3)
                           " (k=" (rtos (cdr scale) 2 6)
                           ", fmt=" (car scale) ")")))
        (progn
          (setq *ap-scale* 1.0)
          (ap-warn "Масштаб не определён — использую 1.0")))
      ;; 2-й проход: принять/отклонить
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i)
              obj (vlax-ename->vla-object ent)
              on (vl-catch-all-apply 'vla-get-ObjectName (list obj)))
        (if (vl-catch-all-error-p on) (setq on "?"))
        (setq total (1+ total))
        (cond
          ((or (= on "AcDbPolyline") (= on "AcDb2dPolyline"))
           (setq closed (ap-is-closed-true obj))
           (setq bb (ap-get-bbox obj))
           (if bb
             (progn
               (setq w (caddr bb) h (cadddr bb))
               (setq fmt (ap-format-by-scale w h *ap-scale*))
               (if (and closed fmt)
                 (progn
                   (setq orient (if (> w h) "Альбомная" "Книжная"))
                   (setq *ap-frame-data*
                         (cons (list (strcat "Лист " tab ": " fmt
                                             " [" orient "]")
                                     (car bb) (cadr bb)
                                     fmt orient tab 0)
                               *ap-frame-data*))
                   (setq *ap-frames* (cons fmt *ap-frames*))
                   (setq cnt (1+ cnt)))
                 (setq rejected (1+ rejected))))))
          ((= on "AcDbBlockReference")
           (setq bb (ap-get-bbox obj))
           (if bb
             (progn
               (setq w (caddr bb) h (cadddr bb))
               (setq closed (ap-count-closed (vla-get-Name obj) 0))
               (setq fmt (ap-format-by-scale w h *ap-scale*))
               (ap-trc (strcat "    INSERT '" (vla-get-Name obj)
                               "' w=" (rtos w 2 4)
                               " h=" (rtos h 2 4)
                               " closed=" (itoa closed)
                               " fmt=" (if fmt fmt "nil")))
               (if (and fmt (>= closed 2))
                 (progn
                   (setq orient (if (> w h) "Альбомная" "Книжная"))
                   (setq *ap-frame-data*
                         (cons (list (strcat "Лист " tab ": " fmt
                                             " [" orient "]")
                                     (car bb) (cadr bb)
                                     fmt orient tab 0)
                               *ap-frame-data*))
                   (setq *ap-frames* (cons fmt *ap-frames*))
                   (setq cnt (1+ cnt)))
                 (setq rejected (1+ rejected)))))))
        (setq i (1+ i)))))
  (ap-info (strcat "Принято: " (itoa cnt)
                   ", отклонено: " (itoa rejected)
                   ", всего: " (itoa total)))
  (princ (strcat "Принято: " (itoa cnt)
                 ", отклонено: " (itoa rejected)
                 ", всего: " (itoa total) "\n")))
(princ "[D10] ap-find-current OK\n")

;;; ---------- Дедупликация ----------
(defun ap-bbox-equal (a b tol / mn-a mx-a mn-b mx-b)
  (setq mn-a (nth 1 a) mx-a (nth 2 a)
        mn-b (nth 1 b) mx-b (nth 2 b))
  (and (listp mn-a) (listp mx-a) (listp mn-b) (listp mx-b)
       (< (abs (- (car mn-a) (car mn-b))) tol)
       (< (abs (- (cadr mn-a) (cadr mn-b))) tol)
       (< (abs (- (car mx-a) (car mx-b))) tol)
       (< (abs (- (cadr mx-a) (cadr mx-b))) tol)))

(defun ap-dedup-frames ( / out seen fd)
  (setq out '())
  (foreach fd *ap-frame-data*
    (if (not (vl-some '(lambda (x) (ap-bbox-equal x fd 1.0)) seen))
      (progn
        (setq seen (cons fd seen))
        (setq out (cons fd out)))))
  (setq *ap-frame-data* (reverse out))
  (ap-dbg (strcat "После дедупликации: " (itoa (length *ap-frame-data*)))))

(defun ap-find-all ()
  (setq *ap-frame-data* '() *ap-frames* '())
  (if (= 1 (getvar "TILEMODE"))
    (ap-find-model)
    (ap-find-current))
  (ap-dedup-frames))
(princ "[D11] ap-find-all OK\n")

;;; ---------- DCL ----------
(defun ap-update-list ( / fd)
  (start_list "frames")
  (foreach fd (reverse *ap-frame-data*)
    (if (and fd (car fd))
      (add_list (strcat (if (= (nth 6 fd) 1) "[ ] " "[X] ") (car fd)))))
  (end_list))

(defun ap-create-dcl ( / dcl_file f)
  (setq dcl_file (vl-filename-mktemp "ap" nil ".dcl"))
  (setq f (open dcl_file "w"))
  (write-line "ap_dialog:dialog{" f)
  (write-line "label=\"AutoPlot v157.0\";" f)
  (write-line ":list_box{key=\"frames\";" f)
  (write-line "label=\"Список рамок ([X] — печатать, [ ] — пропустить):\";" f)
  (write-line "height=14;width=80;" f)
  (write-line "multiple_select=true;" f)
  (write-line "}" f)
  (write-line ":row{" f)
  (write-line ":button{key=\"remove\";label=\"Убрать\";}" f)
  (write-line ":button{key=\"toggle\";label=\"Вкл/Выкл\";}" f)
  (write-line "}" f)
  (write-line ":row{" f)
  (write-line ":button{key=\"print\";label=\"ПЕЧАТЬ В PDF\";is_default=true;}" f)
  (write-line ":button{key=\"cancel\";label=\"Закрыть\";is_cancel=true;}" f)
  (write-line "}" f)
  (write-line ":text{key=\"status\";label=\"\";}" f)
  (write-line "}" f)
  (close f)
  dcl_file)
(princ "[D12] dcl OK\n")

;;; ---------- Печать ----------
(defun ap-plot-full (tab fmt orient pt1 pt2 pdf / ok old-fd old-cd)
  (setq ok nil)
  (if (findfile pdf)
    (progn (vl-file-delete pdf)
           (ap-dbg (strcat "Удалён старый PDF: " pdf))))
  (setq old-fd (getvar "FILEDIA") old-cd (getvar "CMDDIA"))
  (setvar "FILEDIA" 0)
  (setvar "CMDDIA" 0)
  (ap-info (strcat "PLOT tab=" tab " fmt=" fmt " -> " pdf))
  (ap-send "_.-PLOT")
  (ap-send "Да")
  (ap-send tab)
  (ap-send "DWG To PDF.pc3")
  (ap-send fmt)
  (ap-send "Миллиметры")
  (ap-send orient)
  (ap-send "Нет")
  (ap-send "Рамка")
  (ap-send pt1)
  (ap-send pt2)
  (ap-send "Вписать")
  (ap-send "Центрировать")
  (ap-send "Да")
  (ap-send "")
  (ap-send "Нет")
  (ap-send "Нет")
  (ap-send "Нет")
  (ap-send "Нет")
  (ap-send pdf)
  (ap-send "Нет")
  (ap-send "Да")
  (setvar "FILEDIA" old-fd)
  (setvar "CMDDIA" old-cd)
  (if (findfile pdf)
    (progn (ap-dbg (strcat "PDF создан: " pdf)) (setq ok t))
    (ap-err (strcat "PDF не создан: " pdf)))
  ok)

(defun ap-print-one-frame (fd dir i / mn mx fmt orient tab st
                              pt1 pt2 pdf result dx dy)
  (if (null fd) nil
    (progn
      (setq mn (cadr fd) mx (caddr fd)
            fmt (nth 3 fd) orient (nth 4 fd)
            tab (nth 5 fd) st (nth 6 fd))
      (if (/= st 0) nil
        (progn
          (setq dx (* 0.01 (- (car mx) (car mn)))
                dy (* 0.01 (- (cadr mx) (cadr mn))))
          (setq pt1 (strcat (rtos (- (car mn) dx) 2 4) ","
                            (rtos (- (cadr mn) dy) 2 4)))
          (setq pt2 (strcat (rtos (+ (car mx) dx) 2 4) ","
                            (rtos (+ (cadr mx) dy) 2 4)))
          (setq pdf (strcat dir "\\" (ap-format-num i) ".pdf"))
          (setq result (ap-plot-full tab (ap-format-full fmt)
                                     orient pt1 pt2 pdf))
          result)))))

(defun ap-do-print ( / path dir prefix i fd pdf ok err total counter)
  (setq ok 0 err 0 i 0)
  (setq path (getfiled "Сохранить PDF" "" "pdf" 1))
  (if (null path)
    (progn (ap-warn "Путь не выбран.") (princ "\nПуть не выбран."))
    (progn
      (setq dir (vl-filename-directory path))
      (setq prefix (vl-filename-base (getvar "DWGNAME")))
      (if (= prefix "") (setq prefix "Drawing"))
      (setvar "BACKGROUNDPLOT" 0)
      (setq total (length *ap-frame-data*))
      (ap-info (strcat "=== ПЕЧАТЬ: " (itoa total) " ==="))
      (ap-info (strcat "Папка: " dir))
      (princ (strcat "\n=== ПЕЧАТЬ: " (itoa total) " ===\n"))
      (foreach fd *ap-frame-data*
        (setq i (1+ i))
        (setq counter (nth 6 fd))
        (if (= counter 1)
          (progn
            (ap-dbg (strcat "[" (itoa i) "] пропущена"))
            (princ (strcat "[" (itoa i) "] пропущена\n")))
          (progn
            (setq pdf (strcat dir "\\" prefix "_"
                              (ap-format-num (1+ ok)) ".pdf"))
            (princ (strcat "[" (itoa i) "] "))
            (if (ap-print-one-frame fd dir i)
              (progn
                (ap-info (strcat "[" (itoa i) "] OK — " pdf))
                (princ (strcat "OK — " pdf "\n"))
                (setq ok (1+ ok)))
              (progn
                (ap-err (strcat "[" (itoa i) "] ОШИБКА — файл не создан"))
                (princ "ОШИБКА — файл не создан\n")
                (setq err (1+ err)))))))
      (ap-info (strcat "=== ГОТОВО: OK=" (itoa ok)
                       " ERR=" (itoa err) " ==="))
      (ap-info (strcat "Папка: " dir))
      (princ (strcat "\n=== ГОТОВО: OK=" (itoa ok)
                     " ERR=" (itoa err) " ===\n"))
      (ap-save-log))))

(defun ap-do-remove ( / sel idx)
  (setq sel (get_tile "frames"))
  (if (/= sel "")
    (progn
      (setq sel (read (strcat "(" sel ")")))
      (foreach idx (reverse sel)
        (setq *ap-frames* (vl-remove (nth idx *ap-frames*) *ap-frames*)
              *ap-frame-data* (vl-remove (nth idx *ap-frame-data*)
                                          *ap-frame-data*)))
      (ap-update-list)
      (set_tile "status" (strcat "Осталось: "
                                  (itoa (length *ap-frames*)))))))

(defun ap-do-toggle-list ( / sel idx fd)
  (setq sel (get_tile "frames"))
  (if (/= sel "")
    (progn
      (setq sel (read (strcat "(" sel ")")))
      (foreach idx (reverse sel)
        (setq fd (nth idx *ap-frame-data*))
        (if fd
          (setq *ap-frame-data*
                (subst (append (list (nth 0 fd) (nth 1 fd) (nth 2 fd)
                                     (nth 3 fd) (nth 4 fd) (nth 5 fd)
                                     (if (= (nth 6 fd) 0) 1 0)))
                       fd *ap-frame-data*))))
      (ap-update-list)
      (set_tile "status" "Статус обновлён"))))
(princ "[D13] print OK\n")

;;; ---------- Основные команды ----------
(defun c:AUTOPLOT ( / dcl_file dcl_id result)
  (vl-load-com)
  (setq *log-buffer* '() *log-counts* '(0 0 0 0 0) *log-errors* '())
  (setq *log-level* 4)
  (ap-info "========================================")
  (ap-info (strcat "AutoPlot v157.0 — лог "
                   (menucmd "M=$(edtime,$(getvar,date),DD.MM.YYYY HH:MM:SS)")))
  (ap-info "========================================")
  (setq *ap-frames* '() *ap-frame-data* '())
  (ap-find-all)
  (if (= (length *ap-frames*) 0)
    (progn
      (ap-warn "Рамки не найдены на активной вкладке.")
      (princ "\nРамки не найдены на активной вкладке.\n"))
    (progn
      (setq dcl_file (ap-create-dcl))
      (setq dcl_id (load_dialog dcl_file))
      (if (new_dialog "ap_dialog" dcl_id)
        (progn
          (ap-update-list)
          (set_tile "status" (strcat "Найдено: "
                                      (itoa (length *ap-frames*))))
          (action_tile "remove" "(ap-do-remove)")
          (action_tile "toggle" "(ap-do-toggle-list)")
          (action_tile "print" "(done_dialog 1)")
          (action_tile "cancel" "(done_dialog 0)")
          (setq result (start_dialog))
          (unload_dialog dcl_id)
          (if (= result 1) (ap-do-print)))
        (progn
          (ap-err "Не удалось открыть диалог.")
          (princ "\nНе удалось открыть диалог.")))
      (vl-file-delete dcl_file)))
  (ap-save-log)
  (princ))
(princ "[D14] c:AUTOPLOT OK\n")

;;; ---------- APFRAMES ----------
(defun c:APFRAMES ( / tab tile ss i ent obj on bb w h fmt closed
                      cnt_ins cnt_lwp cnt_pol cnt_line
                      ok_cnt rej_cnt nm enm cur name_map scale r)
  (setq *diag-buffer* '())
  (ap-diag "========================================")
  (ap-diag " APFRAMES — диагностика рамок")
  (ap-diag (strcat " " (menucmd "M=$(edtime,$(getvar,date),DD.MM.YYYY HH:MM:SS)")))
  (ap-diag "========================================")
  (setq tab (getvar "CTAB"))
  (setq tile (getvar "TILEMODE"))
  (ap-diag (strcat "Активная вкладка: " tab))
  (ap-diag (strcat "TILEMODE: " (itoa tile)
                   " (" (if (= tile 1) "Модель" "Лист") ")"))
  (ap-diag (strcat "INSUNITS: "  (itoa (getvar "INSUNITS"))))
  (ap-diag (strcat "LUNITS: "    (itoa (getvar "LUNITS"))))
  (ap-diag (strcat "LUPREC: "    (itoa (getvar "LUPREC"))))
  (ap-diag (strcat "DIMSCALE: "  (rtos (getvar "DIMSCALE") 2 4)))
  (ap-diag (strcat "CANNOSCALE: "(getvar "CANNOSCALE")))
  (ap-diag "")

  (setq ss (ssget "_X" (list (cons 410 tab))))
  (ap-diag (strcat "Всего объектов на вкладке: "
                   (if ss (itoa (sslength ss)) "0")))
  (setq ss (ssget