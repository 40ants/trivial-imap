(defpackage #:trivial-imap/core
  (:nicknames #:trivial-imap)
  (:use #:cl)
  ;; (:import-from :sanitize)
  ;; (:import-from :mel-base)
  (:import-from :cl-mime)
  (:import-from :babel)
  (:import-from :cl-rfc2047)
  (:import-from :cl-strings)
  (:import-from #:cl-date-time-parser
                #:parse-date-time)
  (:import-from #:local-time
                #:universal-to-timestamp)
  (:import-from #:cl-arrows
                #:->)
  (:import-from #:alexandria
                #:make-keyword)
  (:export
   #:email
   #:get-uid
   #:get-headers
   #:get-subject
   #:get-timestamp
   #:get-text
   #:get-html
   #:fetch-messages))
(in-package trivial-imap/core)


(defun parse-time (string)
  (let ((universal (parse-date-time string)))
    (universal-to-timestamp universal)))


;; (defun read-headers (message)
;;   (let ((stream (mel.public:message-header-stream message)))
;;     (unwind-protect
;;          (mel.mime:read-rfc2822-header stream)
;;       (close stream))))


;; (defun parse-from-address (text)
;;   (let* ((decoded-text (cl-rfc2047:decode* text))
;;          (address (mel.mime:parse-rfc2822-address decoded-text))
;;          (name (sf.mel:eml-address->name address))
;;          (email (sf.mel:eml-address->email address)))
;;     (list :name name :email email)))


;; (defun trim-newlines-and-spaces (text)
;;   (string-trim '(#\Newline #\Return #\Space) text))


;; (defun normalize-newlines (text)
;;   (cl-strings:replace-all text
;;                           (coerce '(#\Return #\Newline) 'string)
;;                           (string #\Newline)))


;; (defun get-usable-text (message)
;;   (let* ((parts (mel.mime:parts message))
;;          (html-parts (remove-if-not #'sf.mel:part-body-html? parts))
;;          (text-parts (remove-if-not #'sf.mel:part-body-text? parts))
;;          (text (cond
;;                  (html-parts (sf.mel:part-body-html (first html-parts)))
;;                  (text-parts (sf.mel:part-body-text (first text-parts)))
;;                  (t nil))))
;;     (when text
;;       (normalize-newlines
;;        (trim-newlines-and-spaces
;;         (sanitize:clean text sanitize:+basic+))))))


;; (defun fetch-message (message)
;;   "Returns a list with two items:

;; 1) Email author's name and email like (:name \"Some Author\" :email \"author@example.com\")
;; 2) Sanitized message body."
  
;;   (let* ((headers (read-headers message))
;;          (subject (alexandria:assoc-value headers :subject))
;;          (parsed-subject (cl-rfc2047:decode* subject)))
    
;;     (let* ((text (get-usable-text message))
;;            (from (alexandria:assoc-value headers :from))
;;            (message-id (alexandria:assoc-value headers :message-id))
;;            (time (alexandria:assoc-value headers :date)))

;;       (list :from (parse-from-address from)
;;             :subject parsed-subject
;;             :message-id message-id
;;             :time (parse-time time)
;;             :text text))
;;     ;; (if (string-equal parsed-subject
;;     ;;                   "Уведомление об изменении статуса заявления в электронном реестре")
;;     ;;     (log:info "dsad")
;;     ;;     (log:info "Skipping" parsed-subject))
;;     ))


;; (defun fetch-messages-from (&key host port username password)
;;   (let* ((imap (apply
;;                 #'mel.folders.imap:make-imaps-folder
;;                 :host host
;;                 :username username
;;                 :password password
;;                 (when port
;;                   (list :port port))))
         
;;          (messages (mel.public:messages imap)))
;;     (mapcar #'fetch-message messages)))

;; (defun search-messages-from (&key host port username password)
;;   (let* ((imap (apply
;;                 #'mel.folders.imap:make-imaps-folder
;;                 :host host
;;                 :username username
;;                 :password password
;;                 (when port
;;                   (list :port port))))
         
;;          (messages (mel.folders.imap::search-mailbox imap
;;                                                      "from \"<noreply-contingent@mos.ru>\""
;;                                                      ;; "Уведомление об изменении статуса заявления в электронном реестре" 
;;                                                      )))
;;     (mapcar #'fetch-message messages)))


;; (defun test-fetch (&key host port username password)
;;   (let ((folder (apply
;;                  #'mel.folders.imap:make-imaps-folder
;;                  :host host
;;                  :username username
;;                  :password password
;;                  (when port
;;                    (list :port port)))))
;;     (mel.folders.imap::fetch-message folder 120584)))


;; (defun create-contact-and-feed-item (message)
;;   (let* ((from (getf message :from))
;;          (email (getf from :email))
;;          (name (getf from :name))
;;          (subject (getf from :subect))
;;          (message-id (getf from :message-id))
;;          (time (getf from :time))
;;          (text (getf from :text)))
;;     (list email name text)))


;; (defun process-messages ()
;;   "This function fetches new emails, creates contacts and feed items."
;;   (let* ((accounts (ubiquitous:value :hacrm/plugins/email :accounts))
;;          (messages
;;            (loop for account in accounts
;;                  append (apply #'fetch-messages-from account))))
;;     (unless accounts
;;       (error "No accounts"))
;;     (loop for message in messages
;;           collect (create-contact-and-feed-item message))))


(defclass email ()
  ((uid :initarg :uid
        :reader get-uid)
   (mime :initarg :mime
         :reader get-mime)
   (headers :initarg :headers
            :reader get-headers)
   (raw :initarg :raw
        :reader get-raw)))


(defun get-subject (email)
  (check-type email email)
  (-> (get-headers email)
      (alexandria:assoc-value :subject)
      (cl-rfc2047:decode*)))


(defun get-timestamp (email)
  (check-type email email)
  (-> (get-headers email)
      (alexandria:assoc-value :date)
      (parse-time)))


(defmethod print-object ((obj email) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "uid=~A subject=\"~A\""
            (get-uid obj)
            (get-subject obj))))


(defun %get-decoded-content (mime)
  "This function returns a mime's context as a text.
   cl-mime:decode-content return a byte vector and here
   we decode it into the unicode and normalize newlines."
  
  (let ((charset (-> (cl-mime:get-header mime :charset)
                     (second)
                     (string-upcase)
                     (make-keyword))))
    (flet ((decode-octets-if-needed (obj)
             "cl-mime:decode-content can return a string if Content-Transfer-Encoding: 7bit
            or a vector if it doesn't. We need a normal unicode text here."
             (etypecase obj
               (string obj)
               (vector (babel:octets-to-string obj
                                               :encoding charset)))))
     
      (-> (cl-mime:decode-content mime)
          (decode-octets-if-needed)
          (cl-strings:replace-all (coerce (list #\Return #\Newline) 'string)
                                  (coerce (list #\Newline) 'string))))))

(defun get-text (email)
  (check-type email email)
  (let ((mime (get-mime email)))
    (labels ((check (mime)
               (let ((content-type (cl-mime:content-type mime))
                     (content-subtype (cl-mime:content-subtype mime)))
                 (cond
                   ((string-equal content-type
                                  "multipart")
                    (loop for part in (cl-mime:content mime)
                          do (check part)))
                   ((and (string-equal content-type
                                       "text")
                         (string-equal content-subtype
                                       "plain"))
                    (return-from get-text
                      (%get-decoded-content mime)))))))
      (check mime))))


(defun get-html (email)
  (check-type email email)
  (let ((mime (get-mime email)))
    (labels ((check (mime)
               (let ((content-type (cl-mime:content-type mime))
                     (content-subtype (cl-mime:content-subtype mime)))
                 (cond
                   ((string-equal content-type
                                  "multipart")
                    (loop for part in (cl-mime:content mime)
                          do (check part)))
                   ((and (string-equal content-type
                                       "text")
                         (string-equal content-subtype
                                       "html"))
                    (return-from get-html
                      (%get-decoded-content mime)))))))
      (check mime))))




(defun get-parts-content-types (email)
  "Helper fro debugging."
  (check-type email email)
  (let ((mime (get-mime email))
        (content-types nil))
    (labels ((check (mime)
               (let ((content-type (cl-mime:content-type mime)))
                 (cond
                   ((string-equal content-type
                                  "multipart")
                    (loop for part in (cl-mime:content mime)
                          do (check part)))
                   (t
                    (push content-type content-types))))))
      (check mime)
      (values content-types))))



(defun make-email (uid raw-data)
  (let ((headers (with-input-from-string (s raw-data)
                   (cl-mime:parse-headers s))))
    (make-instance 'email
                   :uid uid
                   :raw raw-data
                   :headers headers
                   :mime (cl-mime:parse-mime raw-data))))


(defun skip (uids skip-to-uid)
  (do ()  
      ((or (null uids)
            (> (car uids)
               skip-to-uid))
       uids)
    (setf uids
          (cdr uids))))


(defun fetch-messages (host user password
                       &key
                         (folder "Inbox")
                         (timeout 15)
                         (ssl t)
                         (starttls t)
                         (query :all)
                         (limit 10)
                         (since-uid nil))
  (let ((mb (net.post-office:make-imap-connection (list host
                                                        :ssl ssl
                                                        :starttls starttls)
                                                  :user user
                                                  :password password
                                                  :timeout timeout)))
    (net.post-office:select-mailbox mb folder)
    (let* ((uids (net.post-office:search-mailbox mb query :uid t))
           (uids (if (and uids since-uid)
                     (skip uids since-uid)
                     uids))
           (total (length uids))
           (uids (if limit
                     (subseq uids 0 (min limit
                                         (length uids)))
                     uids)))
      (values
       (loop for uid in uids
             for email = (make-email uid (net.post-office:fetch-letter mb uid :uid t))
             collect email)
       total))))
