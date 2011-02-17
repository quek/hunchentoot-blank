(in-package #:hunchentoot-blank)

(defparameter *default-directory*
  (pathname (directory-namestring #.(or *compile-file-truename*
                                        *load-truename*)))
  "このファイルがあるディレクトリ")

(defparameter *js-path* (merge-pathnames "js/" *default-directory*)
  "JavaScript 用ディレクトリ")
(defparameter *css-path* (merge-pathnames "css/" *default-directory*)
  "スタイルシート用ディレクトリ")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; DB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
シェルから次のコマンドを実行してデータベースを作成してください。
echo 'create database hunchentoot_blank default character set utf8;' | mysql -u root
|#

(clsql-sys:file-enable-sql-reader-syntax)

(defparameter *connection-spec* '("localhost" "hunchentoot_blank" "root" "")
  "MySQL の接続情報。(DBサーバ DB名 ユーザ パスワード)")

(defmacro with-db (&body body)
  (alexandria:with-gensyms (res handler-done)
    `(clsql:with-database (clsql:*default-database*
                           *connection-spec*
                           :make-default t
                           :pool t
                           :encoding :utf-8
                           :database-type :mysql)
       ;; for debug
       (clsql-sys::start-sql-recording)
       (unwind-protect
            (let (,res (,handler-done t))
              ;; (clsql:execute-command "SET NAMES 'utf8'")
              (clsql-sys:with-transaction (:database clsql:*default-database*)
                ;; hunchentoot:redirect した場合の対応
                (catch 'hunchentoot::handler-done
                  (setf ,res (progn ,@body))
                  (setf ,handler-done nil)))
              (if ,handler-done
                  (throw 'hunchentoot::handler-done nil)
                  ,res))
         ;; for debug
         (clsql-sys::stop-sql-recording)))))
;;(with-db (clsql-sys:query "select 'あ'"))

(clsql-sys:def-view-class user ()
  ((id :accessor id
       :initarg :id
       :db-kind :key
       :db-constraints :auto-increment
       :type integer)
   (email :accessor email
          :initarg :email
          :db-constraints :unique
          :type string)
   (password :initarg :password
             :initarg :plain-password
             :type string)))

(defmethod initialize-instance :after ((user user)
                                        &key plain-password
                                        &allow-other-keys)
  "make-instance で :plain-password が指定されていた場合、
password に hash-password したものを設定する。"
  (when plain-password
    (setf (password user) plain-password)))

(defun hash-password (password)
  "パスワードのハッシュ関数"
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (ironclad:ascii-string-to-byte-array password))))

(defmethod (setf password) (password (user user))
  "パスワードのハッシュをセットする。"
  (setf (slot-value user 'password) (hash-password password)))

(defun authenticate (email password)
  (let ((password (hash-password password)))
    (car
     (clsql:select 'user
                   :flatp t
                   :where [and [= [email] email] [= [password] password]]))))
;; (authenticate "user1@example.com" "password")

;; テーブル作成
(with-db
  (unless (clsql-sys:table-exists-p 'user)
    (clsql-sys:create-view-from-class 'user)
    'user))
#+テーブル削除
(progn
  (clsql-sys:drop-view-from-class 'user))

#+テストデータ作成
(with-db
  (let ((user (make-instance 'user :email "user1@example.com"
                             :plain-password "password")))
    (clsql-sys:update-records-from-instance user)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Web server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf
 ;; for utf-8
 hunchentoot:*hunchentoot-default-external-format* (flexi-streams:make-external-format :utf-8)
 hunchentoot:*default-content-type* "text/html; charset=utf-8"
 ;; for debug
 hunchentoot:*catch-errors-p* nil)

(setf hunchentoot:*dispatch-table*
      (list
       'hunchentoot:dispatch-easy-handlers
       (hunchentoot:create-folder-dispatcher-and-handler "/css/" *css-path*)
       (hunchentoot:create-folder-dispatcher-and-handler "/js/" *js-path*)))

(defvar *acceptor*)

(defun start (&optional (port 8888))
  "Web サーバ起動"
  (setf *acceptor* (hunchentoot:start
                    (make-instance 'hunchentoot:acceptor
                                   :port port))))

(defun stop ()
  "Web サーバ停止"
  (hunchentoot:stop *acceptor*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ビュー
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *login-user* nil "ログインユーザ")

(defun login (user redirect-url)
  "ログイン処理"
  (setf *login-user* user)
  (when hunchentoot:*session*
    (hunchentoot:remove-session hunchentoot:*session*))
  (hunchentoot:start-session)
  (setf (hunchentoot:session-value 'login-user-id) (id user))
  (hunchentoot:redirect redirect-url))

(defun logout (redirect-url)
  "ログアウト処理"
  (when hunchentoot:*session*
    (hunchentoot:remove-session hunchentoot:*session*))
  (setf *login-user* nil)
  (hunchentoot:redirect redirect-url))

(setf *prologue* "<!DOCTYPE html>")
(setf *attribute-quote-char* #\")

(defmacro with-default-template ((&key (title "題名")
                                       (charset "UTF-8")) &body body)
  "ページのテンプレート"
  `(with-html-output-to-string (out nil :prologue t :indent t)
     (htm (:html :lang "ja"
                 (:head
                  (:meta :charset ,charset)
                  (:title ,title)
                  (:link :rel "stylesheet" :href"css/main.css" :media "all")
                  (:script :src "http://ajax.googleapis.com/ajax/libs/jquery/1/jquery.min.js"))
                 (:body ,@body)))))

(defun select-login-user ()
  (let ((login-user-id (hunchentoot:session-value 'login-user-id)))
    (when login-user-id
      (caar (clsql:select 'user
                          :where [= [id] login-user-id])))))

(defmacro with-login-user (&body body)
  `(let ((*login-user* (select-login-user)))
     ,@body))

(defmacro define-page (description lambda-list &body body)
  "ページ定義。

description
hunchentoot:define-easy-handler に加えて
ログインが必要な場合は :login-require-p t を指定する。

lambda-list
hunchentoot:define-easy-handler と同じ。"
  (let ((login-required-p (and (listp description)
                               (getf (cdr description) :login-require-p))))
    (when (listp description)
      (remf (cdr description) :login-require-p))
    `(hunchentoot:define-easy-handler ,description ,lambda-list
       (with-db
         (with-login-user
           ,@(when login-required-p
               `((unless *login-user*
                   ;; ログインしていな場合の処理
                   (hunchentoot:redirect "/"))))
           ,@body)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 各ページ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; トップページ
(define-page (%root :uri "/") ()
  (with-default-template (:title "トップ")
    (htm
     (:div :class "ba" "ブランクプロジェクト")
     (if *login-user*
         (htm (:div (str (email *login-user*))
                    " でログインしています。")
              (:div (:a :href "logout" "ログアウト")))
         (htm (:div (:a :href "login" "ログイン"))))
     (htm (:div (:a :href "/secret" "ログインが必要なページへのリンク"))))))

;;;; ログインページ
(define-page (%login :uri "/login") (email messages)
  (with-default-template (:title "ログイン")
    (htm
     (when messages
       (htm (:ul (loop for message in messages
                       do (htm (:li (str message)))))))
     (:form :action "authenticate" :method :post
            (:div "email"
                  (:input :type :text :name "email" :value email))
            (:div "パスワード"
                  (:input :type :password :name "password"))
            (:div (:input :type :submit :value "ログイン"))))))

;;;; 認証
(define-page (%authenticate :uri "/authenticate")
    ((email :init-form "") (password :init-form ""))
  (let (messages)
    (when (string= "" email)
      (push "email を入力してください。" messages))
    (when (string= "" password)
      (push "パスワードを入力してください。" messages))
    (if messages
        (%login :email email :messages (reverse messages))
        (let ((user (authenticate email password)))
          (if user
              (login user "/")
              (%login :email email))))))

;;;; ログアウト
(define-page (%logout :uri "/logout") ()
  (logout "/"))

;;;; ログインが必要なページ
(define-page (secrect :uri "/secret" :login-require-p t) ()
  (with-default-template (:title "秘密のページ")
   (htm (:p "このページはログインが必要なページです。"))))
