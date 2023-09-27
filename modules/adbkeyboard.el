;;; adbkeyboard.el --- FIXME description  -*- lexical-binding: t; -*-
;; Copyright (C) 2023 Ziqi Yang <mr.meowking@anche.no>

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; NOTE install https://github.com/senzhk/ADBKeyBoard on your phone first
;; and check https://blog.csdn.net/qq13933506749/article/details/119535618

;;; Code:

(defun mk/adb-send-message (str)
  (interactive (list (read-string "TEXT: ")))
  (let* ((strlen (length str))
          (sublen 10)
          (index 0))
    (while (length> str index)
      (call-process-shell-command
        (format
          "adb shell am broadcast -a ADB_INPUT_B64 --es msg (echo -n '%s' | base64)"
          (substring str index (min strlen (+ index sublen)))))
      (setq index (+ index sublen))))
  (sit-for .5)
  (mk/adb-wechat-press-send-button))

(defun mk/adb-send-messages ()
  (interactive)
  (let (str)
    (while 1
      (setq str (read-string "TEXT: "))
      (mk/adb-send-message str))))

(defun mk/adb-wechat-press-send-button ()
  (interactive)
  (call-process-shell-command
    "adb shell input tap 960 2246"))

(defun mk/adb-switch-back-keyboard ()
  (interactive)
  (call-process-shell-command
    "adb shell ime set com.google.android.inputmethod.latin/com.android.inputmethod.latin.LatinIME"))

(defun mk/adb-switch-to-adbkeyboard ()
  (interactive)
  (call-process-shell-command
    "adb shell ime set com.android.adbkeyboard/.AdbIME"))

(defun mk/adb--delete-char ()
  (interactive)
  (call-process-shell-command
    "adb shell am broadcast -a ADB_INPUT_CODE --ei code 67")
  (set-transient-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "x") #'mk/adb--delete-char)
      map)))

(provide 'adbkeyboard)

;;; adbkeyboard.el ends here
