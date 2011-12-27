;;; 01-server.el --- server settings

;; Copyright (C) 2011  Jon-Michael Deldin

;; Author: Jon-Michael Deldin <dev@jmdeldin.com>
;; Keywords: convenience

;;; Code:

(load "server")
(unless (server-running-p)
  (server-start))

(provide '01-server)
;;; 01-server.el ends here
