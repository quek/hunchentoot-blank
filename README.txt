何かの時に必要になるかもしれないと思い、
Common Lisp で Web アプリを作るためのブランクプロジェクトを作ってみました。

Hunchentoot, CL-WHO, CLSQL を使ったブランクプロジェクトです。

MySQL を使っています。
シェルから次のコマンドを実行してデータベースを作成してください。

echo 'create database hunchentoot_blank default character set utf8;' | mysql -u root

次を実行して http://localhost:8888/ にアクセスしてください。

(hunchentoot-blank::start)
