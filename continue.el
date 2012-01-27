
(require 'fuzzy-match)

(defvar continue-buffer-word-frequency-table (make-hash-table :test 'equal))
(make-local-variable 'continue-buffer-word-frequency-table)

(defvar continue-buffer-frequencies-computed-tick -1)
(make-local-variable 'continue-buffer-frequencies-computed-tick)

(defvar continue-buffer-fuzzy-char-list nil)
(make-local-variable 'continue-buffer-fuzzy-char-list)

(defvar continue-buffer-fuzzy-char-list-computed-tick -1)
(make-local-variable 'continue-buffer-fuzzy-char-list-computed-tick)

(defun continue-zip (&rest lists)
  (let* (;;(lists (append (list a) rest))
         (n (- (length lists) 1))
         (i 0)
         (rs '()))
    (while (some 'identity (mapcar (lambda (l) (> (length l) i)) lists))
      (setq rs (append rs (list (loop for m from 0 to n
                                      collect (nth i (nth m lists))))))
      (setq i (1+ i)))
    rs))


(defun continue-sourcemarker-p (smarker)
  "Test if SMARKER is a sourcemarker."
  (when smarker
    (condition-case nil
        (when (and (assoc :point smarker)
                   (assoc :file smarker)
                   (assoc :lines-center smarker)
                   (assoc :lines-above smarker)
                   (assoc :lines-below smarker))
          t)
      (error nil))))


(defun continue-ignore-line-p ()
  (save-excursion
    (beginning-of-line)
    (or (looking-at "^\\s-*$")
        (< (length (let ((r (string-to-list (buffer-substring (point-at-bol) (point-at-eol)))))
                     (dolist (c r (concatenate 'string r))
                       (if (eq c ? )
                           (setq r (cdr r)))))) 3))))

(defun continue-previous-line-string ()
  ;; if already at beginning of buffer collect #bobp#
  (if (save-excursion
        (beginning-of-line)
        (bobp))
      "#bobp#"
    (progn
      ;; walk up one line
      (previous-line)
      ;; skip empty lines or none when current line is not empty
      (while (and (continue-ignore-line-p)
                  (not (save-excursion
                         (beginning-of-line)
                         (bobp))))
        (previous-line))
      ;; check again if skipping empty lines
      ;; brought us to the beginning of the buffer
      ;; also, check if the actual line is empty
      ;; because skippieng terminates on bobp as well
      ;; and if it did, we still want to collect the line
      ;; instead of #bobp#
      ;; also, this is the part where the line is collected
      ;; the condition will only be true on empty lines
      ;; (which should have been skipped by now)
      ;; or if we are at bobp
      (if (and (save-excursion
                 (beginning-of-line)
                 (bobp))
               (continue-ignore-line-p))
          "#bobp#"
        (buffer-substring-no-properties (point-at-bol) (point-at-eol))))))

(defun continue-next-line-string ()
  (if (save-excursion
        (end-of-line)
        (eobp))
      "#eobp#"
    (progn
      (next-line)
      (while (and (continue-ignore-line-p)
                  (not (save-excursion
                         (end-of-line)
                         (eobp))))
        (next-line))
      (if (and (save-excursion
                 (end-of-line)
                 (eobp))
               (continue-ignore-line-p))
          "#eobp#"
        (buffer-substring-no-properties (point-at-bol) (point-at-eol))))))

(defun continue-previous-line ()
  (interactive)
  (previous-line)
  (let ((direction 'up))
    (while (continue-ignore-line-p)
      (when (save-excursion
              (beginning-of-line)
              (bobp))
        (setq direction 'down))
      (if (eq direction 'up)
          (previous-line)
        (next-line))
      )))

(defun continue-next-line ()
  (interactive)
  (next-line)
  (let ((direction 'down))
    (while (continue-ignore-line-p)
      (when (save-excursion
              (end-of-line)
              (eobp))
        (setq direction 'up))
      (if (eq direction 'down)
          (next-line)
        (previous-line))
      )))

(defun continue-sourcemarker-create (&optional n)
  "Sourcemarkers are a persitent alternative to emacs markers specifically aimed
at marking lines in source code.

Creating a sourcemarker will collect lines around the current point which will
then be used by `continue-sourcemarker-restore' to restore point regardless of
whether the piece of code has been moved around in the file. It should be even
possible to restore a point if the lines that represent that point in the
sourcemarker have partly changed in the file."
  (let (r)
    (unless (and n (>= n 2))
      (setq n 2))
    (with-current-buffer (or (buffer-base-buffer (current-buffer))
                             (current-buffer))
      (save-window-excursion
        (save-excursion
          (save-restriction
            (org-save-outline-visibility nil
                (show-all)
              ;; return nil if the buffer is not big enough for a mark
              (cond ((save-excursion
                       (end-of-buffer)
                       (< (line-number-at-pos) (+ (* n 2) 1)))
                     `(,(progn
                          (beginning-of-buffer)
                          (point-at-bol))
                       ,(buffer-file-name (current-buffer))
                       nil))
                    (t
                     (progn
                       ;; move point to nearest non-empty line
                       ;; handle end-of-buffer/beginning-of-buffer
                       ;; by reversing the search direction
                       (let ((rev nil))
                         (while (continue-ignore-line-p)
                           (when (save-excursion
                                   (end-of-line)
                                   (eobp))
                             (setq rev t))
                           (if rev
                               (previous-line)
                             (next-line))
                           ))
                       (beginning-of-line)
                       ;; two functions walking up/down from current point collecting lines
                       ;; trimming whitespaces, skipping empty lines, collecting #eobp#/#bobp#
                       ;; when at end/beginning of buffer
                       (flet ((collect-up (m) (save-excursion
                                                (reverse
                                                 ;; arg m is number of lines to collect
                                                 (loop for i from 1 to m
                                                       collect (continue-previous-line-string)))))
                              (collect-down (m) (save-excursion
                                                  (loop for i from 1 to m
                                                        collect (continue-next-line-string)))))
                         (let ((above (collect-up n))
                               (below (collect-down n))
                               (line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
                           (setq r `((:point . ,(point-at-bol))
                                     (:file . ,(buffer-file-name (current-buffer)))
                                     (:number-of-lines . ,(line-number-at-pos (point-max)))
                                     (:lines-center . ,line)
                                     (:lines-above . ,above)
                                     (:lines-below . ,below)
                                     ))
                           ))))))))))
    r))

(defun continue-string-to-words-only (str)
  (split-string (concat (remove-if (lambda (a)
                                     (some (lambda (b) (eq a b))
                                           (concat "abcdefghijklmnopqrstuvwxyz"
                                                   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                                   " "))) str)) " " t))

(defun continue-buffer-word-frequencies (&optional buf)
  (unless buf
    (setq buf (current-buffer)))
  (when (> (buffer-modified-tick) continue-buffer-frequencies-computed-tick)
    (let ((frequencies (make-hash-table :test 'equal))
          (w-count 0)
          (words (with-current-buffer buf
                   (continue-string-to-words-only (buffer-string)))))
      (mapcar (lambda (s) (puthash s (1+ (or (gethash s frequencies nil)
                                             (progn
                                               (setq w-count (1+ w-count))
                                               0)))  frequencies)) words)
      (maphash (lambda (k v)
                 (puthash k (/ (float v) (float w-count)) frequencies))
               frequencies)
      (setq continue-buffer-frequencies-computed-tick (buffer-modified-tick)
            continue-buffer-word-frequency-table frequencies)))
  continue-buffer-word-frequency-table)

(defun continue-string-uniqueness (str)
  (let ((frequencies (continue-buffer-word-frequencies))
        (words (continue-string-to-words-only str))
        (score 0.0))
    (mapcar (lambda (w) (let* ((freq (gethash w frequencies 0.0)))
                          (setq score (+ score (/ freq (length words)))))) words)
    (- 1.0 score)))

(defun continue-string-shannon-entropy (str)
  (let* ((frequencies (continue-buffer-word-frequencies))
         (freqs (mapcar (lambda (w) (gethash w frequencies 0.0)) (continue-string-to-words-only str)))
         (bits 0.0))
    (dolist (f freqs (abs (/ bits (log 2))))
      (setq bits (+ bits (* f (log f)))))))

(defun continue-string-words-by-uniqueness (str)
  (let ((frequencies (continue-buffer-word-frequencies))
        (words (split-string str " " t)))
    (sort (continue-zip words (mapcar 'continue-string-uniqueness words))
          (lambda (a b) (> (second a) (second b))))))

(defun continue-sourcemarker-simple-search (smarker &optional matches)
  (print "simple")
  (save-excursion
    (when (continue-sourcemarker-p smarker)
      (let* ((point (cdr (assoc :point smarker)))
             (point-at-bol (save-excursion (goto-char point) (point-at-bol)))
             (file (cdr (assoc :file smarker)))
             (line-2 (nth 1 (cdr (assoc :lines-above smarker))))
             (line-1 (nth 0 (cdr (assoc :lines-above smarker))))
             (line (cdr (assoc :lines-center smarker)))
             (line+1 (nth 0 (cdr (assoc :lines-below smarker))))
             (line+2 (nth 1 (cdr (assoc :lines-below smarker)))))
        (progn
          (goto-char point)
          (if (looking-at (regexp-quote line))
              point-at-bol
            (progn
              (goto-char point)
              (continue-next-line)
              (if (looking-at (regexp-quote line+1))
                  point-at-bol
                (progn
                  (goto-char point)
                  (continue-previous-line)
                  (if (looking-at (regexp-quote line-1))
                      point-at-bol
                    (progn
                      (goto-char point)
                      (continue-next-line)
                      (continue-next-line)
                      (if (looking-at (regexp-quote line+2))
                          point-at-bol
                        (progn
                          (goto-char point)
                          (continue-previous-line)
                          (continue-previous-line)
                          (if (looking-at (regexp-quote line-2))
                              point-at-bol
                            nil))))))))))))))

(defun test-simple-search ()
  (interactive)
  (print (continue-sourcemarker-simple-search (gethash (buffer-file-name (current-buffer)) continue-db))))

(defun* continue-sourcemarker-regexp-search-1 (smarker &optional matches min-length)
  (print "regexp")
  (save-excursion
    (if (and matches return-all)
        matches
      (when (continue-sourcemarker-p smarker)
        (let* ((point (cdr (assoc :point smarker)))
               (line (cdr (assoc :lines-center smarker)))
               (words (remove-if (lambda (w) (or (< (length w) (or min-length 5))
                                                 (< (let ((hs (make-hash-table)))
                                                      (mapcar (lambda (c) (puthash c 0 hs)) w)
                                                      (hash-table-count hs)) 4)
                                                 ))
                                 (sort (split-string line " " t) (lambda (a b) (> (length a) (length b))))))
               (current-nol (line-number-at-pos (point-max)))
               (smarker-nol (or (cdr (assoc :number-of-lines smarker)) -1))
               (last-forward-match point)
               (last-backward-match point)
               (direction (if (>= current-nol smarker-nol)
                              'forward
                            'backward)))
          (while (cond ((and (or (eq direction 'forward)
                                 (eq last-backward-match 'finished))
                             (not (eq last-forward-match 'finished)))
                        (if (block "word-loop-forward"
                              (goto-char last-forward-match)
                              (dolist (w words nil)
                                (let ((re (regexp-quote w)))
                                  (if (re-search-forward re nil t)
                                      (return-from "word-loop-forward" t)
                                    (goto-char last-forward-match)))))
                            (progn
                              (add-to-list 'matches (point-at-bol))
                              (setq last-forward-match (point)
                                    direction 'backward)
                              t)
                          (progn
                            (setq last-forward-match 'finished)
                            t)))
                       ((and (or (eq direction 'backward)
                                 (eq last-forward-match 'finished))
                             (not (eq last-backward-match 'finished)))
                        (if (block "word-loop-backward"
                              (goto-char last-backward-match)
                              (dolist (w words nil)
                                (let ((re (regexp-quote w)))
                                  (if (re-search-backward re nil t)
                                      (return-from "word-loop-backward" t)
                                    (goto-char last-backward-match)))))
                            (progn
                              (add-to-list 'matches (point-at-bol))
                              (setq last-backward-match (point)
                                    direction 'forward)
                              t)
                          (progn
                            (setq last-backward-match 'finished)
                            t)))
                       (t nil)))
          matches)))))

(defun* continue-sourcemarker-regexp-search (smarker &optional matches min-length)
  (print "regexp")
  (save-excursion
    (if (and matches return-all)
        matches
      (when (continue-sourcemarker-p smarker)
        (let* ((file (cdr (assoc :file smarker)))
               (point (cdr (assoc :point smarker)))
               (line (cdr (assoc :lines-center smarker)))
               (words (remove-if (lambda (w) (or (< (length w) (or min-length 5))
                                                 (< (let ((hs (make-hash-table)))
                                                      (mapcar (lambda (c) (puthash c 0 hs)) w)
                                                      (hash-table-count hs)) 4)
                                                 ))
                                 (sort (split-string line " " t) (lambda (a b) (> (length a) (length b))))))
               (last-matches nil))
          (setq matches (or matches (reverse (continue-sourcemarker-regexp-search-1 smarker matches min-length))))
          (while (and (> (length matches) 1)
                      words)
            (setq last-matches matches)
            (let ((re (regexp-quote (pop words))))
              (dolist (m matches)
                (goto-char m)
                (unless (string-match re (buffer-substring (point-at-bol) (point-at-eol)))
                  (setq matches (remove-if (lambda (n) (eq m n)) matches))))))
          (unless matches
            (setq matches last-matches))
          ;; (when (and words matches)
          ;;   (let ((p nil)
          ;;         (m (car matches)))
          ;;     (goto-char m)
          ;;     (while (and words
          ;;                 (setq p (string-match (regexp-quote (pop words)) (buffer-substring (point-at-bol) (point-at-eol))))))
          ;;     (unless p (setq matches nil))))
          matches)))))

(defun test-continue-regexp-search ()
  (interactive)
  (print (continue-sourcemarker-regexp-search (gethash (buffer-file-name (current-buffer)) continue-db))))

(defun continue-buffer-string-to-fuzzy-char-lists (&optional seperator)
  (when (> (buffer-modified-tick) continue-buffer-fuzzy-char-list-computed-tick)
    (setq continue-buffer-fuzzy-char-list (FM-strings-to-char-lists (split-string (buffer-string) (or seperator " ")))
          continue-buffer-fuzzy-char-list-computed-tick (buffer-modified-tick)))
  continue-buffer-fuzzy-char-list)

(defun continue-sourcemarker-fuzzy-search (smarker &optional matches score-sym)
  (print "fuzzy")
  (save-excursion
    (when matches
      (setq matches (mapcar 'line-number-at-pos matches)))
    (when (continue-sourcemarker-p smarker)
      (let* ((lines (or (when matches
                          (mapcar (lambda (l) (goto-line l) (FM-string-to-char-list (buffer-substring (point-at-bol) (point-at-eol)))) matches))
                        (continue-buffer-string-to-fuzzy-char-lists "\n")))
             (numbered-lines (continue-zip (or matches
                                               (loop for i from 1 to (length lines) collect i)) lines))
             (point (cdr (assoc :point smarker)))
             (string (FM-string-to-char-list (or str
                                                 (cdr (assoc :lines-center smarker)))))
             (bestfuzz -1)
             (bestline nil))
        (while numbered-lines
          (let* ((tuple (pop numbered-lines))
                 (n (first tuple))
                 (line (second tuple))
                 (thisfuzz (FM-matchiness-intern string line)))
            (cond ((> thisfuzz bestfuzz)
                   (setq bestfuzz thisfuzz
                         bestline n))
                  ((and (eq bestfuzz thisfuzz)
                        bestline
                        (< (abs (- (line-number-at-pos point) n)) (abs (- (line-number-at-pos point) bestline))))
                   (setq bestfuzz thisfuzz
                         bestline n)))))
        (goto-line bestline)
        (when score-sym
          (setf (symbol-value score-sym) bestfuzz)))
      (point-at-bol))))

(defun continue-sourcemarker-combined-search (smarker &optional matches str)
  (or (continue-sourcemarker-simple-search m)
      (let* ((line-2 (nth 1 (cdr (assoc :lines-above smarker))))
             (line-1 (nth 0 (cdr (assoc :lines-above smarker))))
             (line (cdr (assoc :lines-center smarker)))
             (line+1 (nth 0 (cdr (assoc :lines-below smarker))))
             (line+2 (nth 1 (cdr (assoc :lines-below smarker))))
             (threshold (or threshold (/ (length line) 3)))
             (score 0)
             p)
        (setq p (continue-sourcemarker-fuzzy-search m (continue-sourcemarker-regexp-search m nil 5) 'score))
        p)))

(defun continue-sourcemarker-restore (ms &optional threshold)
  (interactive)
  (save-excursion
    (let ((matches nil))
      (dolist (m (if (continue-sourcemarker-p ms) (list ms) ms) (if (continue-sourcemarker-p ms) (car matches) matches))
        (with-current-buffer (find-file-noselect (cdr (assoc :file m)))
          (goto-char (continue-sourcemarker-combined-search m))
          (add-to-list 'matches (point-marker)))))))

(defun continue-sourcemarker-visit (smarker)
  (let* ((m (continue-sourcemarker-restore smarker))
         (buf (marker-buffer m))
         (oldframe (current-frame)))
    (when (markerp m)
      (when (get-buffer-window (get-buffer buf) 'visible)
        (select-frame (window-frame (get-buffer-window (get-buffer buf) 'visible))))
      (goto-char (marker-position m)))))

(defvar continue-db (make-hash-table :test 'equal))
;;(setq continue-db (make-hash-table :test 'equal))

(defvar continue-db-filename "~/.continue-db")

(defvar continue-db-ignore '("\.recentf"
                             ".*/\.mk-project/"))

;; (dolist (buf (buffer-list))
;;   (when (buffer-file-name buf)
;;     (puthash (buffer-file-name buf) mk-proj-sourcemarker continue-db)))

(defun continue-load-db (&optional filename)
  (with-temp-buffer
    (insert-file-contents (or filename continue-db-filename))
    (eval-buffer)))

(defun continue-write-db (&optional filename)
  (with-temp-buffer
    (maphash (lambda (k v)
               (insert (concat "(puthash " (prin1-to-string k) " '" (prin1-to-string v) " continue-db)"))
               (newline)) continue-db)
    (write-file (or filename continue-db-filename))))

(defun continue-save (&optional buf)
  (interactive)
  (save-window-excursion
    (save-restriction
      (let* ((buf (or buf (current-buffer)))
             (filename (buffer-file-name buf)))
        (when filename
          (unless (and (buffer-file-name buf)
                       (some (lambda (re) (string-match re (buffer-file-name buf))) continue-db-ignore))
            (with-current-buffer buf
              (puthash filename (continue-sourcemarker-create) continue-db))))))))

(defun continue-restore (&optional filename)
  (interactive)
  (let ((buf (or (and filename
                      (find-file-noselect filename))
                 (current-buffer))))
    (with-current-buffer buf
      (let* ((filename (buffer-file-name buf))
             (smarker (when filename
                        (gethash filename continue-db nil))))
        (when smarker
          (continue-sourcemarker-visit smarker)
          (when (eq major-mode 'org-mode)
            (org-back-to-heading t)
            (org-reveal)))))))

(eval-after-load "continue"
  '(progn
     (continue-load-db)
     (add-hook 'kill-emacs-hook 'continue-write-db)
     (add-hook 'find-file-hook 'continue-restore)
     (add-hook 'after-save-hook 'continue-save)
     (add-hook 'kill-buffer-hook 'continue-save)
     ))

(provide 'continue)











