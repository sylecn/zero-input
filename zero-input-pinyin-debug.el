;;; zero-input-pinyin-debug.el --- zero-input-pinyin debug utilities -*- lexical-binding: t -*-

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;;

;;; Code:

(require 'zero-input-pinyin)

(defun zero-input-pinyin-build-candidates-test (preedit-str)
  "Test data for testing partial commit.

PREEDIT-STR the preedit string."
  (cond
   ((equal preedit-str "liyifeng")
    (setq zero-input-pinyin-used-preedit-str-lengths '(8 4 4 4 2 2 2))
    '("李易峰" "利益" "礼仪" "离异" "里" "理" "力"))
   ((equal preedit-str "feng")
    (setq zero-input-pinyin-used-preedit-str-lengths '(4 4 4 4 4))
    '("风" "封" "疯" "丰" "凤"))
   ((equal preedit-str "yifeng")
    (setq zero-input-pinyin-used-preedit-str-lengths '(6 6 2 2 2 2))
    '("一封" "遗风" "艺" "依" "一" "以"))
   (t nil)))

(defun zero-input-pinyin-build-candidates (preedit-str fetch-size)
  (zero-input-pinyin-build-candidates-test preedit-str)
  (setq zero-input-fetch-size (max fetch-size (length zero-input-candidates))))

(provide 'zero-input-pinyin-debug)

;;; zero-input-pinyin-debug.el ends here
