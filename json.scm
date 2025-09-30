;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 手写的 JSON 解析器 - 严格 R6RS 兼容版 (修正版)
;;; 导入 R6RS 标准库
(import (rnrs (6)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-json str)
  "公共入口函数：解析一个完整的 JSON 字符串。"
  (call-with-values
      (lambda () (parse-value (string-trim-left str)))
    (lambda (value rest)
      (if (string-null? (string-trim-right rest))
          value
          (error 'parse-json "JSON 解析错误：字符串末尾有额外字符" rest)))))

;;; ----------------------------------------------------
;;; 主分派器
;;; ----------------------------------------------------

(define (parse-value str)
  "根据字符串的第一个字符，分派到相应的解析函数。"
  (let ((s (string-trim-left str)))
    (if (string-null? s)
        (error 'parse-value "JSON 解析错误：需要一个值，但输入为空")
        (let ((first-char (string-ref s 0)))
          (cond
            ((char=? first-char #\{) (parse-object s))
            ((char=? first-char #\[) (parse-array s))
            ((char=? first-char #\") (parse-string s))
            ((string-prefix? "true" s) (values #t (substring s 4 (string-length s))))
            ((string-prefix? "false" s) (values #f (substring s 5 (string-length s))))
            ((string-prefix? "null" s) (values 'null (substring s 4 (string-length s))))
            ((or (char-numeric? first-char) (char=? first-char #\-)) (parse-number s))
            (else (error 'parse-value "JSON 解析错误：无效的值" s)))))))

;;; ----------------------------------------------------
;;; 特定类型的解析函数
;;; ----------------------------------------------------

(define (parse-string str)
  "解析一个 JSON 字符串。"
  (let loop ((i 1) (chars '()))
    (if (>= i (string-length str))
        (error 'parse-string "JSON 解析错误：未闭合的字符串")
        (let ((char (string-ref str i)))
          (cond
            ((char=? char #\")
             (values (list->string (reverse chars))
                     (substring str (+ i 1) (string-length str))))
            ((char=? char #\\)
             (let ((next-char (string-ref str (+ i 1))))
               (loop (+ i 2) (cons next-char chars))))
            (else
             (loop (+ i 1) (cons char chars))))))))

(define (parse-number str)
  "解析一个 JSON 数字。"
  (let loop ((i 0))
    (if (or (>= i (string-length str))
            (not (char-in-set? (string-ref str i) "0123456789.eE+-")))
        (let ((num-str (substring str 0 i)))
          (values (string->number num-str)
                  (substring str i (string-length str))))
        (loop (+ i 1)))))

(define (parse-object str)
  "解析一个 JSON 对象，并将其表示为哈希表。"
  (let-values (((members-list rest)
                (parse-comma-separated
                 (substring str 1 (string-length str))
                 #\}
                 parse-member)))
    ;; 使用 R6RS 标准的 hashtable 函数
    (let ((obj-hashtable (make-hashtable string-hash string=?)))
      (for-each
       (lambda (member-pair)
         (hashtable-set! obj-hashtable (car member-pair) (cdr member-pair)))
       members-list)
      (values obj-hashtable rest))))

(define (parse-array str)
  "解析一个 JSON 数组。"
  (let-values (((elements rest)
                (parse-comma-separated
                 (substring str 1 (string-length str))
                 #\]
                 parse-value)))
    (values (list->vector elements) rest)))

;;; ----------------------------------------------------
;;; 辅助/通用解析逻辑
;;; ----------------------------------------------------

(define (parse-member str)
  "解析一个对象的成员（键值对）。"
  (let* ((s1 (string-trim-left str)))
    (if (string-null? s1)
        (error 'parse-member "JSON 解析错误：对象成员键值对为空")
        (let-values (((key rest1) (parse-string s1)))
          (let* ((s2 (string-trim-left rest1)))
            (if (not (string-prefix? ":" s2))
                (error 'parse-member "JSON 解析错误：对象成员缺少冒号 ':'" s2)
                (let ((s3 (string-trim-left (substring s2 1 (string-length s2)))))
                  (let-values (((value rest2) (parse-value s3)))
                    (values (cons key value) rest2)))))))))

(define (parse-comma-separated str terminal-char item-parser)
  "通用逻辑：解析由逗号分隔的列表，直到遇到 terminal-char。"
  (let ((s (string-trim-left str)))
    (if (string-prefix? (string terminal-char) s)
        (values '() (substring s 1 (string-length s)))
        (let loop ((s s) (items '()))
          (let-values (((item rest1) (item-parser s)))
            (let* ((new-items (cons item items))
                   (s2 (string-trim-left rest1)))
              (cond
                ((string-prefix? (string terminal-char) s2)
                 (values (reverse new-items) (substring s2 1 (string-length s2))))
                ((string-prefix? "," s2)
                 (loop (string-trim-left (substring s2 1 (string-length s2))) new-items))
                (else
                 (error 'parse-comma-separated "JSON 解析错误：期待 ',' 或 '" terminal-char "'" s2)))))))))

;;; ----------------------------------------------------
;;; 工具函数
;;; ----------------------------------------------------
(define (string-trim-left str)
  (let loop ((i 0))
    (if (or (>= i (string-length str)) (not (char-whitespace? (string-ref str i))))
        (substring str i (string-length str))
        (loop (+ i 1)))))

(define (string-trim-right str)
  (let loop ((i (- (string-length str) 1)))
    (if (or (< i 0) (not (char-whitespace? (string-ref str i))))
        (substring str 0 (+ i 1))
        (loop (- i 1)))))

(define (string-null? str)
  (= 0 (string-length str)))

(define (string-prefix? prefix str)
  (let ((prefix-len (string-length prefix))
        (str-len (string-length str)))
    (and (>= str-len prefix-len)
         (string=? prefix (substring str 0 prefix-len)))))

(define (char-in-set? char set-str)
  "检查一个字符是否存在于一个字符串中 (手动循环，保证可移植)。"
  (let loop ((i 0))
    (cond
      ((>= i (string-length set-str)) #f)
      ((char=? char (string-ref set-str i)) #t)
      (else (loop (+ i 1))))))

;;; ----------------------------------------------------
;;; Pretty-print JSON 输出函数
;;; ----------------------------------------------------

(define (make-indent level)
  "生成指定级别的缩进字符串。"
  (let loop ((i 0) (result ""))
    (if (>= i level)
        result
        (loop (+ i 1) (string-append result "  ")))))

(define (pretty-print-json value indent-level)
  "格式化输出 JSON 值。"
  (cond
    ;; 处理哈希表（对象）
    ((hashtable? value)
     (let ((keys (vector->list (hashtable-keys value))))
       (if (null? keys)
           (display "{}")
           (begin
             (display "{\n")
             (let loop ((ks keys) (first #t))
               (if (not (null? ks))
                   (let ((key (car ks)))
                     (if (not first)
                         (display ",\n"))
                     (display (make-indent (+ indent-level 1)))
                     (display "\"")
                     (display key)
                     (display "\": ")
                     (pretty-print-json (hashtable-ref value key #f) (+ indent-level 1))
                     (loop (cdr ks) #f))))
             (display "\n")
             (display (make-indent indent-level))
             (display "}")))))
    
    ;; 处理向量（数组）
    ((vector? value)
     (let ((len (vector-length value)))
       (if (= len 0)
           (display "[]")
           (begin
             (display "[\n")
             (let loop ((i 0))
               (if (< i len)
                   (begin
                     (if (> i 0)
                         (display ",\n"))
                     (display (make-indent (+ indent-level 1)))
                     (pretty-print-json (vector-ref value i) (+ indent-level 1))
                     (loop (+ i 1)))))
             (display "\n")
             (display (make-indent indent-level))
             (display "]")))))
    
    ;; 处理字符串
    ((string? value)
     (display "\"")
     (display value)
     (display "\""))
    
    ;; 处理布尔值
    ((boolean? value)
     (display (if value "true" "false")))
    
    ;; 处理 null
    ((eq? value 'null)
     (display "null"))
    
    ;; 处理数字
    ((number? value)
     (display value))
    
    ;; 未知类型
    (else
     (display "#<unknown>"))))

(define (print-pretty-json value)
  "打印格式化的 JSON。"
  (pretty-print-json value 0)
  (newline))

;;; ----------------------------------------------------
;;; 主程序
;;; ----------------------------------------------------
  
;; 定义我们的复杂 JSON 字符串npm
(define complex-json-string
  "{
    \"person\": {
      \"name\": \"John Doe\",
      \"age\": 30,
      \"isStudent\": false,
      \"courses\": [
        {\"title\": \"History\", \"credits\": 3},
        {\"title\": \"Math\", \"credits\": 4}
      ],
      \"address\": {
        \"street\": \"123 Main St\",
        \"city\": \"Anytown\"
      }
    },
    \"metadata\": {
      \"source\": \"example.json\",
      \"timestamp\": \"2023-10-27T10:00:00Z\"
    }
  }")

;; 使用我们手写的解析器进行解析
(define parsed-data (parse-json complex-json-string))

;; 格式化输出完整的 JSON
(display "=== Pretty-printed JSON ===\n")
(print-pretty-json parsed-data)