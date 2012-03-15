;; continue.el -- continue working on files from where you saved
;; Copyright (C) 2012 Andreas Raster <lazor@affenbande.org>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

(require 'cl)

(condition-case nil (require 'org) (error (defalias 'org-save-outline-visibility 'progn)))

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

(defun continue-ignore-word-p (w &optional min-length)
  "Used to decide if a word should be skipped when creating/restoring
a sourcemarker in `continue.el'

See also `continue-ignore-line-p', `continue-previous-line',
`continue-next-line'"
  (and (< (length (substring-no-properties w)) (or min-length 2))
       (< (let ((hs (make-hash-table)))
            (mapcar (lambda (c) (puthash c 'found hs)) w)
            (hash-table-count hs)) 3)))

(defun continue-ignore-line-p (&optional min-length)
  "Used to decide if a line should be skipped when creating/restoring a
sourcemarker in `continue.el'

See also `continue-ignore-word-p', `continue-previous-line',
`continue-next-line'"
  (save-excursion
    (beginning-of-line)
    (or (looking-at "^\\s-*$")
        (= (length (remove-if (lambda (w) (continue-ignore-word-p w min-length))
                              (split-string (buffer-substring (point-at-bol) (point-at-eol)) " " t))) 0))))










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
  "Goto previous line but skip over lines for which `continue-ignore-line-p'
matches.  This is used in `continue.el' instead of `previous-line'."
  (interactive)
  (unless (save-excursion (beginning-of-line) (bobp))
    (previous-line-nomark)
    (let ((direction 'up))
      (while (continue-ignore-line-p)
        (when (save-excursion
                (beginning-of-line)
                (bobp))
          (setq direction 'down))
        (if (eq direction 'up)
            (previous-line-nomark)
          (next-line-nomark))
        ))))

(defun continue-next-line ()
  "Goto next line but skip over lines for which `continue-ignore-line-p'
matches. This is used in `continue.el' instead of `next-line'. "
  (interactive)
  (unless (save-excursion (end-of-line) (eobp))
    (next-line-nomark)
    (let ((direction 'down))
      (while (continue-ignore-line-p)
        (when (save-excursion
                (end-of-line)
                (eobp))
          (setq direction 'up))
        (if (eq direction 'down)
            (next-line-nomark)
          (previous-line-nomark))
        ))))

(defun continue-looking-at (re)
  "Like `looking-at' but will match \"#eobp#\" for a line at end of buffer
and \"#bobp#\" for a line at beginning of buffer."
  (or (and (string-equal re "#eobp#")
           (save-excursion (goto-char (point-at-eol)) (eobp)))
      (and (string-equal re "#bobp#")
           (save-excursion (goto-char (point-at-bol)) (bobp)))
      (looking-at re)))

(defun continue-re-search-forward (re &optional bound noerror count match-buffer-end)
  "Equivalent for `re-search-forward' in `continue.el'. RE BOUND NOERROR and
COUNT behave the same as in `re-search-forward', when MATCH-BUFFER-END is
non-nil this function will match the end of the buffer if RE equals
\"#eobp#\"."
  (if (and match-buffer-end
           (string-equal re "#eobp#")
           (save-excursion (goto-char (point-at-eol)) (eobp)))
      (point-max) ;;(goto-char (point-max))
    (re-search-forward re bound noerror count)))

(defun continue-re-search-backward (re &optional bound noerror count match-buffer-beginning)
  "Equivalent for `re-search-backward' in `continue.el'. RE BOUND NOERROR and COUNT behave the same
as in `re-search-backward', when MATCH-BUFFER-BEGINNING is non-nil this function will match the beginning
of the buffer if RE equals \"#bobp#\"."
  (if (and match-buffer-beginning
           (string-equal re "#bobp#")
           (save-excursion (goto-char (point-at-bol)) (bobp)))
      (point-min) ;;(goto-char (point-min))
    (re-search-backward re bound noerror count)))








(defun continue-sourcemarker-create (&optional n)
  "Sourcemarkers are a persitent alternative to emacs markers specifically aimed
at marking lines in source code.

Creating a sourcemarker will collect lines around the current point which will
then be used by `continue-sourcemarker-restore' to restore point regardless of
whether the piece of code has been moved around in the file. It should be even
possible to restore a point if the lines that represent that point in the
sourcemarker have partly changed in the file.

Optional parameter N can be used to specify how many lines around will be saved.
The default is 2 meaning two lines above the current line and two line below the
current line will be saved in addition to the current line.

Sourcemarker data structure layout:
`((:point . ,(point-at-bol))
  (:file . ,(buffer-file-name (current-buffer)))
  (:number-of-lines . ,(line-number-at-pos (point-max)))
  (:lines-center . ,line)
  (:lines-above . ,above)
  (:lines-below . ,below)
  (:timestamp . ,(format-time-string \"%s\")))"
  (let (r)
    (unless (and n (>= n 2))
      (setq n 2))
    (with-current-buffer (or (buffer-base-buffer (current-buffer))
                             (current-buffer))
      (save-window-excursion
        (save-excursion
          (save-restriction
            (org-save-outline-visibility t
              (show-all)
              ;; return nil if the buffer is not big enough for a mark
              (cond ((save-excursion
                       (end-of-buffer)
                       (< (line-number-at-pos) (+ (* n 2) 1)))
                     `(,(progn
                          (goto-char (point-min))
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
                                     (:timestamp . ,(format-time-string "%s"))
                                     ))
                           ))))))))))
    r))







(defun continue-sourcemarker-simple-search (smarker)
  "Simply look at the saved lines around the saved point of
SMARKER and return a point if we find any matches comparing
the current lines to the saved lines. Return nil if nothing
matches.

See also `continue-sourcemarker-restore'"
  (save-excursion
    (when (continue-sourcemarker-p smarker)
      (let* ((current-nol (line-number-at-pos (point-max)))
             (smarker-nol (or (cdr (assoc :number-of-lines smarker)) -1))
             ;; we'll check to buffer positions, one computed from the saved number of
             ;; lines of the file and the current number of lines (adding the difference
             ;; to the saved point in hope that it was moved by the additions/deletions)
             (point-1 (+ (cdr (assoc :point smarker)) (- current-nol smarker-nol)))
             (point-at-bol-1 (save-excursion (goto-char point-1) (point-at-bol)))
             ;; and the other point we check is just the saved point, assuming that nothing had
             ;; changed
             (point-2 (cdr (assoc :point smarker)))
             (point-at-bol-2 (save-excursion (goto-char point-2) (point-at-bol)))
             (file (cdr (assoc :file smarker)))
             (line-2 (nth 1 (cdr (assoc :lines-above smarker))))
             (line-1 (nth 0 (cdr (assoc :lines-above smarker))))
             (line (cdr (assoc :lines-center smarker)))
             (line+1 (nth 0 (cdr (assoc :lines-below smarker))))
             (line+2 (nth 1 (cdr (assoc :lines-below smarker)))))
        ;; just try all possible combos, does not care if smarker was created with n > 2
        (progn
          (goto-char point-at-bol-1)
          (if (continue-looking-at (regexp-quote line))
              point-at-bol-1
            (progn
              (goto-char point-at-bol-1)
              (continue-next-line)
              (if (continue-looking-at (regexp-quote line+1))
                  point-at-bol-1
                (progn
                  (goto-char point-at-bol-1)
                  (continue-previous-line)
                  (if (continue-looking-at (regexp-quote line-1))
                      point-at-bol-1
                    (progn
                      (goto-char point-at-bol-1)
                      (continue-next-line)
                      (continue-next-line)
                      (if (continue-looking-at (regexp-quote line+2))
                          point-at-bol-1
                        (progn
                          (goto-char point-at-bol-1)
                          (continue-previous-line)
                          (continue-previous-line)
                          (if (continue-looking-at (regexp-quote line-2))
                              point-at-bol-1
                            (progn
                              (goto-char point-at-bol-2)
                              (if (continue-looking-at (regexp-quote line))
                                  point-at-bol-2
                                (progn
                                  (goto-char point-at-bol-2)
                                  (continue-next-line)
                                  (if (continue-looking-at (regexp-quote line+1))
                                      point-at-bol-2
                                    (progn
                                      (goto-char point-at-bol-2)
                                      (continue-previous-line)
                                      (if (continue-looking-at (regexp-quote line-1))
                                          point-at-bol-2
                                        (progn
                                          (goto-char point-at-bol-2)
                                          (continue-next-line)
                                          (continue-next-line)
                                          (if (continue-looking-at (regexp-quote line+2))
                                              point-at-bol-2
                                            (progn
                                              (goto-char point-at-bol-2)
                                              (continue-previous-line)
                                              (continue-previous-line)
                                              (if (continue-looking-at (regexp-quote line-2))
                                                  point-at-bol-2
                                                nil))))))))))))))))))))))))

(defun test-simple-search ()
  (interactive)
  (print (continue-sourcemarker-simple-search (gethash (buffer-file-name (current-buffer)) continue-db))))

(defun* continue-sourcemarker-regexp-search (smarker &optional (matching-order '((:lines-center)
                                                                                 (:lines-above . 1)
                                                                                 (:lines-below . 1)
                                                                                 (:lines-above . 2)
                                                                                 (:lines-below . 2))) last-final-score token-search-state)
  "Find SMARKER in current buffer using regular expressions (re-search-forward
and -backward). MATCHING-ORDER can be used to specify which lines
of the sourcemarker to search and in which order.

The default:
'((:lines-center)
  (:lines-above . 1)
  (:lines-below . 1)
  (:lines-above . 2)
  (:lines-below . 2))

tries to find the center line first, then the one above that,
then below, then two above and lastly the one two below.

LAST-FINAL-SCORE and TOKEN-SEARCH-STATE are used to save the
search state when this function is called recursively.

See also `continue-sourcemarker-restore'."
  ;; matching-order is a list of 'tokens' that represent the matches found for a line of the sourcemarker,
  ;; so e.g. matches-center represents the matches where any word from the center-line
  ;; matched, matches-above . 2 are all matches where the line 2 lines above the center-line
  ;; matched, etc.
  ;;
  ;; so each token represents a list of points, integer numbers, which in turn represent the
  ;; line where the match was found in the current buffer
  ;;
  ;; now the first token is used as a pivot, all remaining token matches are normalized
  ;; so that the point numbers are shifted according to how many line jumps the token that
  ;; represents them is away from the pivot
  ;; for example if the matches-center token is the pivot (the default), then all matches found
  ;; for the line above the center line will be shifted downwards one line so that their points
  ;; WILL line up with the center line IF there is a match for the center line of the
  ;; same sourcemarker at that buffer position as well
  ;;
  ;; so after normalizing those matches from one line above, ALL matches from the center line
  ;; AND ALL matches from the line above that point to the SAME buffer position, represent
  ;; the SAME sourcemarker
  ;;
  ;; so then, to find a position with as much matching lines around it as possible, we only
  ;; have to look at the normalized matches and find those buffer positions for which as most
  ;; matches as possible are equal
  ;;
  ;; example: we have two matches for the center-line, at buffer position 10 and 20
  ;; we found a matching word from the center line
  ;; the only other matches we found for our sourcemarker were matches for second line
  ;; below the center-line, for which we found a match at position 15
  ;; so now we look at our one match for the second line and since it is the second line
  ;; above the center line we shift it two lines upward, and we might end up at 10, the
  ;; same position as one of our matches for the center line, which means those two belong
  ;; together
  ;; we can then assume that buffer position 10 is a better match for our sourcemarker
  ;; then position 20
  ;;
  ;; token-search-state keeps all neccessary information to resume search in another recursion step
  ;;
  ;; restore token- hashtables from token-search-state or initialize with empty hashtables
  (let* ((token-last-forward-match (or (cdr (assoc :token-last-forward-match token-search-state))
                                       (make-hash-table :test 'equal)))
         (token-last-backward-match (or (cdr (assoc :token-last-backward-match token-search-state))
                                        (make-hash-table :test 'equal)))
         (token-direction (or (cdr (assoc :token-direction token-search-state))
                              (make-hash-table :test 'equal)))
         (token-cache (or (cdr (assoc :token-cache token-search-state))
                          (make-hash-table :test 'equal)))
         (token-cache-pos (or (cdr (assoc :token-cache-pos token-search-state))
                              (make-hash-table :test 'equal)))
         ;; check number of lines saved in smarker vs number of lines in current buffer, adjust point
         ;; so it is where our smarker should be according to the difference between the two number-of-lines
         (current-nol (line-number-at-pos (point-max)))
         (smarker-nol (or (cdr (assoc :number-of-lines smarker)) -1))
         (point (save-excursion (goto-char (+ (cdr (assoc :point smarker)) (- current-nol smarker-nol))) (point-at-bol))))
    (flet ((token-line (tok)
                       ;; map token to line in smarker
                       (cond ((eq (car tok) :lines-center)
                              (cdr (assoc (car tok) smarker)))
                             ((eq (car tok) :lines-below)
                              (nth (- (cdr tok) 1) (cdr (assoc (car tok) smarker))))
                             ((eq (car tok) :lines-above)
                              (nth (- (cdr tok) 1) (reverse (cdr (assoc (car tok) smarker)))))))
           (line-to-words (line)
                          (remove-if 'continue-ignore-word-p
                                     (split-string line " " t)))
           (match-token-difference (a b)
                                   ;; given two tokens a and b find the number of line jumps it would
                                   ;; take to get from a to b
                                   (flet ((token-value (tok)
                                                       (cond ((eq (car tok) :lines-center)
                                                              0)
                                                             (t (if (eq (car tok) :lines-below)
                                                                    (cdr tok)
                                                                  (* (cdr tok) -1))))))
                                     (- (token-value a) (token-value b))
                                     ))
           (token-next-match (tok)
                             ;; for token TOK, return the next match, will return nil if no match can be
                             ;; found and then restart from the first match if called a second time
                             ;;
                             ;; this searches matches concentrical around the smarker point, every call
                             ;; returns a single match and advances the token-search-state so that the next
                             ;; call will return the next match for the token
                             ;; matches are cached in a list and returned every revolving iteration by getting
                             ;; the element of the list last-cache-pos points to, if it points beyond
                             ;; the end of the cache this function tries to find another match with re-search-
                             ;; forward/backward
                             ;;
                             ;; last-forward-match and last-backward-match are used to keep the position of the
                             ;; last match, so it can be restored when we want to continue searching
                             ;; if we fail to find a match in a direction, we set last-forward/backward-match
                             ;; to finished indicating that we found all matches in that direction
                             ;; (note that in that case this function calls itself recursivly so searching
                             ;; continues in the other direction that is not yet finished, so we don't
                             ;; return nil unless there is really nothing left to find)
                             ;;
                             ;; when both directions are finished (and last-cache-pos pointing beyond the cache
                             ;; end), we return nil ONCE so that any while loop using this function stops, but
                             ;; we also set last-cache-pos to 0 so that if we call this function again with tok,
                             ;; we'll start getting the cached elements
                             (let* ((words (line-to-words (token-line tok)))
                                    (last-forward-match (gethash tok token-last-forward-match point))
                                    (last-backward-match (gethash tok token-last-backward-match point))
                                    (direction (gethash tok token-direction (if (eq (car tok) :lines-above)
                                                                                'backward
                                                                              'forward)))
                                    (cache (gethash tok token-cache nil))
                                    (last-cache-pos (gethash tok token-cache-pos -1))
                                    (result (cond ((and (>= last-cache-pos 0)
                                                        (< last-cache-pos (- (length cache) 1)))
                                                   (progn
                                                     (let ((p last-cache-pos))
                                                       (setq last-cache-pos (+ last-cache-pos 1))
                                                       (nth p cache))))
                                                  ((and (or (eq direction 'forward)
                                                            (eq last-backward-match 'finished))
                                                        (not (eq last-forward-match 'finished)))
                                                   ;; look through all words try to find a match, search every word
                                                   ;; pick the match closest to smarker point
                                                   ;; special case if eobp or bobp, return 'eobp/'bobp instead of match,
                                                   ;; receiver has to handle that
                                                   (let ((obp-or-match (block "word-loop-forward"
                                                                         (cond ((and (= (length words) 1)
                                                                                     (string-equal (car words) "#eobp#"))
                                                                                (return-from "word-loop-forward" 'obp))
                                                                               ((let ((ms (loop for w in words
                                                                                                if (progn (goto-char last-forward-match)
                                                                                                          (continue-re-search-forward (regexp-quote w) nil t nil t))
                                                                                                collect (point))))
                                                                                  (when ms
                                                                                    (goto-char (car (sort ms (lambda (a b) (< (abs (- a point)) (abs (- b point)))))))
                                                                                    t
                                                                                    ))
                                                                                (return-from "word-loop-forward" 'match))
                                                                               (t (goto-char last-forward-match))))))
                                                     ;; obp-or-match will either
                                                     ;; 'match: at (point) is a match
                                                     ;; 'obp: we hit the end or beginning of the buffer
                                                     ;; nil in case nothing was found
                                                     (cond ((eq obp-or-match 'match)
                                                            (progn
                                                              (setq last-forward-match (point-at-eol)
                                                                    direction 'backward)
                                                              (setq cache (append cache `(,(point-at-bol))))
                                                              (setq last-cache-pos (- (length cache) 1))
                                                              (point-at-bol)))
                                                           ((eq obp-or-match 'obp)
                                                            (progn
                                                              (setq last-forward-match 'finished)
                                                              (setq cache (append cache '(eobp)))
                                                              (setq last-cache-pos (- (length cache) 1))
                                                              'eobp))
                                                           (t
                                                            (progn
                                                              (setq last-forward-match 'finished)))))
                                                   )
                                                  ((and (or (eq direction 'backward)
                                                            (eq last-forward-match 'finished))
                                                        (not (eq last-backward-match 'finished)))
                                                   (let ((obp-or-match (block "word-loop-backward"
                                                                         (cond ((and (= (length words) 1)
                                                                                     (string-equal (car words) "#bobp#"))
                                                                                (return-from "word-loop-backward" 'obp))
                                                                               ((let ((ms (loop for w in words
                                                                                                if (progn (goto-char last-backward-match)
                                                                                                          (continue-re-search-backward (regexp-quote w) nil t nil t))
                                                                                                collect (point))))
                                                                                  (when ms
                                                                                    (goto-char (car (sort ms (lambda (a b) (< (abs (- a point)) (abs (- b point)))))))
                                                                                    t
                                                                                    ))
                                                                                (return-from "word-loop-backward" 'match))
                                                                               (t (goto-char last-backward-match))))))
                                                     (cond ((eq obp-or-match 'match)
                                                            (progn
                                                              (setq last-backward-match (point-at-bol)
                                                                    direction 'forward)
                                                              (setq cache (append cache `(,(point-at-bol))))
                                                              (setq last-cache-pos (- (length cache) 1))
                                                              (point-at-bol)))
                                                           ((eq obp-or-match 'obp)
                                                            (progn
                                                              (setq last-backward-match 'finished)
                                                              (setq cache (append cache '(bobp)))
                                                              (setq last-cache-pos (- (length cache) 1))
                                                              'bobp))
                                                           (t
                                                            (progn
                                                              (setq last-backward-match 'finished))))))
                                                  (t (progn
                                                       (setq last-cache-pos 0)
                                                       nil)))))
                               ;; save the token-search-state in hashtables
                               (puthash tok last-forward-match token-last-forward-match)
                               (puthash tok last-backward-match token-last-backward-match)
                               (puthash tok direction token-direction)
                               (puthash tok cache token-cache)
                               (puthash tok last-cache-pos token-cache-pos)
                               (if (eq result 'finished)
                                   (token-next-match tok)
                                 result)
                               )))
      (let* ((pivot-token (car matching-order))
             (first-match (token-next-match pivot-token)))
        ;; only if there are matches for the pivot line and tokens for
        ;; other matches are still available (other than the first one which
        ;; we use as pivot) do the main comparing
        (cond ((and first-match
                    (cdr matching-order))
               ;; MAINLOOP
               ;; gathering matches, normalizing and comparing them
               (let ((results nil)
                     (counter 0))
                 (flet ((normalize (m tok)
                                   (let ((d (match-token-difference tok pivot-token)))
                                     (save-excursion
                                       (cond ((eq m 'eobp)
                                              (save-excursion (goto-char (point-max)) (point-at-bol)))
                                             ((eq m 'bobp)
                                              (point-min))
                                             (t
                                              (goto-char m)
                                              (loop for i from 0 to (- (abs d) 1)
                                                    do (if (< d 0)
                                                           (progn
                                                             (continue-next-line))
                                                         (progn
                                                           (continue-previous-line))))
                                              (point-at-bol)))))))
                   (block "match-testing-loop"
                     (let (tm done-first)
                       (while (setq tm (or (unless done-first
                                             (setq done-first first-match))
                                           (token-next-match pivot-token)))
                         (let ((tm-score 1))
                           (dolist (against-token (cdr matching-order))
                             (block "against-match-testing-loop"
                               (let ((am (token-next-match against-token)))
                                 (if am
                                     (let ((am-normalized (normalize am against-token)))
                                       (when (= tm am-normalized)
                                         (setq tm-score (+ tm-score 1))
                                         (return-from "against-match-testing-loop" t)))
                                   (return-from "against-match-testing-loop" t)))))
                           (setq results (append results `((,tm-score . ,tm))))
                           ;; go fast rather than correct if there are many matches
                           ;; tweak mainloop breaking here for snappier behaviour in edge cases
                           (when (or (= tm-score (length matching-order))
                                     (> counter 100)
                                     (and (> counter 10)
                                          (>= tm-score (+ (/ (length matching-order) 2) 1)))
                                     )
                             (return-from "match-testing-loop" t))
                           (setq counter (+ counter 1))))))
                   ;; sort matches by their score, and if score is equal sort by distance from sourcemarker center point (smaller is better)
                   (let* ((final-result (car (sort results (lambda (a b) (cond ((> (car a) (car b))
                                                                                t)
                                                                               ((= (car a) (car b))
                                                                                (if (< (abs (- point (cdr a))) (abs (- point (cdr b))))
                                                                                    t
                                                                                  nil)))))))
                          (final-score (car final-result))
                          (final-match (cdr final-result)))
                     ;; recur without pivot element if final score is too low, hoping that we will find a better match
                     (if (and last-final-score
                              (>= last-final-score final-score))
                         ;; previous recursion steps score looked better, just return nil so calling function returns
                         ;; final-match instead of recur-match
                         nil
                       (if (and (< final-score (+ (/ (length matching-order) 2) 1))
                                (> (length (cdr matching-order)) 1)
                                (not last-final-score))
                           (let ((recur-match (continue-sourcemarker-regexp-search smarker (cdr matching-order) final-score `((:token-cache . ,token-cache)
                                                                                                                              (:token-cache-pos . ,(maphash (lambda (k v) 0) token-cache-pos))
                                                                                                                              ))))
                             ;; if recursion yields nothing return final-score
                             ;; unless final-score is 1 then retun nil
                             (or recur-match
                                 (unless (< final-score (+ (/ (length matching-order) 2) 1))
                                   final-match)))
                         (unless (< final-score (+ (/ (length matching-order) 2) 1))
                           final-match)))
                     ))))
              ;; END OF MAINLOOP
              ;;
              ;; when we only have matches for the pivot line, but no other matches to
              ;; compare with, then just return the first match for the pivot line
              ((and first-match
                    (not (cdr matching-order)))
               (unless (> last-final-score 1)
                 first-match))
              ;; when there are no matches for the pivot line but (potentially) other
              ;; matches for the rest of the matching order, call this function recursivly
              ;; with the first token of the matching order (the current pivot) stripped
              ;; from the matching order
              ((and (not first-match)
                    (cdr matching-order)
                    (> (length (cdr matching-order)) 1)
                    (not last-final-score))
               (continue-sourcemarker-regexp-search smarker (cdr matching-order) nil `((:token-cache . ,token-cache)
                                                                                       (:token-cache-pos . ,(maphash (lambda (k v) 0) token-cache-pos))
                                                                                       )))
              ;; we failed to find anything at all
              (t nil))))))

(defun test-regexp-search ()
  (interactive)
  (print (gethash (buffer-file-name (current-buffer)) continue-db))
  (let ((p (or (continue-sourcemarker-regexp-search (gethash (buffer-file-name (current-buffer)) continue-db)) (point-min))))
    (print p)
    (goto-char p)))











(defun continue-sourcemarker-restore (ms)
  "Restore a list of sourcemarkers. MS can be a list or a single
sourcemarker. This tries to restore the sourcemarker by calling
`continue-sourcemarker-simple-search' and if that fais it tries
`continue-sourcemarker-regexp-search'.
Returns a list of points where sourcemarkers were found.

See alse `continue-sourcemarker-visit'."
  (interactive)
  (save-excursion
    (let ((matches nil))
      (dolist (m (if (continue-sourcemarker-p ms) (list ms) ms) (if (continue-sourcemarker-p ms) (car matches) matches))
        (with-current-buffer (find-file-noselect (cdr (assoc :file m)))
          (org-save-outline-visibility nil
            (show-all)
            (goto-char (or (continue-sourcemarker-simple-search m)
                           (continue-sourcemarker-regexp-search m)
                           (point-min)))
            (add-to-list 'matches (point))))))))

(defun continue-sourcemarker-visit (smarker)
  "Visit SMARKER, opening the file if necessary but only
visiting the point returned by `continue-sourcemarker-restore',
not displaying the buffer."
  (let* ((m (continue-sourcemarker-restore smarker))
         (buf (find-file-noselect (cdr (assoc :file smarker))))
         (oldframe (current-frame)))
    (when m
      (when (get-buffer-window (get-buffer buf) 'visible)
        (select-frame (window-frame (get-buffer-window (get-buffer buf) 'visible))))
      (goto-char m))))










(defvar continue-db (make-hash-table :test 'equal)
  "A hashtable that holds the saved sourcemarkers.")

(defvar continue-db-filename "~/.continue-db"
  "The filename where the `continue-db' is stored.")

(defvar continue-db-ignore '("\.recentf"
                             ".*/\.mk-project/"
                             ".*/\.continue-db"
                             ".*/\.recentf"
                             "/tmp/.*"
                             ".*\.tmp")
  "List of regular expression. Every file that matches is ignored by
`continue-save'.")

(defun continue-load-db (&optional filename)
  "Load data from `continue-db-filename' or FILENAME and put it
into `continue-db'."
  (when (file-exists-p continue-db-filename)
    (with-temp-buffer
      (insert-file-contents (or filename continue-db-filename))
      (eval-buffer))))

(defun continue-write-db (&optional filename)
  "Take `continue-db' serialize it and write it to
`continue-db-filename' or FILENAME."
  (with-temp-buffer
    (maphash (lambda (k v)
               (when v
                 (insert (concat "(puthash " (prin1-to-string k) " '" (prin1-to-string v) " continue-db)"))
                 (newline))) continue-db)
    (write-file (or filename continue-db-filename))))

(defun continue-save (&optional buf)
  "Create sourcemarker for current position in current buffer or BUF,
put it into `continue-db'."
  (interactive)
  (unless (boundp 'continue-prevent-save)
    (save-window-excursion
      (save-restriction
        (org-save-outline-visibility nil
          (let* ((buf (or buf (current-buffer)))
                 (filename (buffer-file-name buf)))
            (when filename
              (unless (and (buffer-file-name buf)
                           (some (lambda (re) (string-match re (buffer-file-name buf))) continue-db-ignore))
                (with-current-buffer buf
                  (puthash filename (continue-sourcemarker-create) continue-db))
                ;;(continue-write-db)
                ))))))))

(defun continue-restore (&optional filename)
  "Look up current buffers filename or FILENAME in `continue-db'
and restore associated sourcemarker, if any."
  (interactive)
  (unless (boundp 'continue-prevent-restore)
    (let ((buf (or (and filename
                        (find-file-noselect filename))
                   (current-buffer))))
      (with-current-buffer buf
        (let* ((filename (buffer-file-name buf))
               (smarker (when filename
                          (gethash filename continue-db nil))))
          (when smarker
            (continue-sourcemarker-visit smarker)
            (when (and (eq major-mode 'org-mode)
                       (condition-case nil (org-back-to-heading t) (error nil)))
              (org-reveal))
            ))))))

(eval-after-load "continue"
  '(progn
     (continue-load-db)

     (add-hook 'kill-emacs-hook 'continue-write-db)
     (add-hook 'find-file-hook 'continue-restore)
     (add-hook 'after-save-hook 'continue-save)
     (run-with-idle-timer 60 t 'continue-write-db)
     ;;(add-hook 'kill-buffer-hook 'continue-save)

     ;;(unless (boundp 'org-save-outline-visibility)
     ;;  (defalias 'org-save-outline-visibility 'progn))
     ))

(provide 'continue)

