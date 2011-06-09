これは何
========
xyzzy で動く [Clack] インターフェースな HTTP Server。

  [Clack]: http://clacklisp.org/

遊びというか試しに作ってみた程度の物なので、いろいろアレです。

インストール
============

NetInstaller から
-----------------
<del>[カフェイン中毒] からどうぞ。</del>

  [カフェイン中毒]: http://bowbow99.sakura.ne.jp/xyzzy/packages.l

設定
====
今のところなし

使い方
======
現状（2011-06-09 時点）での動かし方。
これで localhost:8000 へのアクセスを待って、誰かがアクセスしてきたら何か
返します。

    ;; simple clack webapp
    (require "cmu_loop")
    
    (defun display-environment (environ)
      `(200
        (:content-type "text/plain")
        (,(with-output-to-string (*standard-output*)
            (princ "The Environment:\n")
            (loop for (key value) on environ by #'cddr
              do (format t " ~S~24T= ~S~%" key value))))))
    
    ;; create and start xlack server
    (load "path/to/xlack.l")
    (defvar *server* (xlack::create-server 'display-environment
                                           :port 8000))
    (xlack::start-server *server*)

止める方法。
今のところ  server 止めないまま server オブジェクトを見失うと（xyzzy を終
了する以外に）止める方法が無いので気を付けてください。

    ;; stop xlack server
    (xlack::stop-server *server*)


注意点、既知の問題など
======================
* server 止めないまま server オブジェクトを見失うと（xyzzy を終了する以外
  に）止める方法が無いので気を付けてください（大事な事なので２どいいｍ

バグ報告、質問、要望などは [GitHubIssues] か [@bowbow99] あたりへお願いします。

  [GitHubIssues]: http://github.com/bowbow99/xyzzy.xlack/issues
  [@bowbow99]: http://twitter.com/bowbow99
