brakesim
========
###使い方
1. シェルを2つ起動（以降s1とs2と呼ぶ）
2. s1でmake
3. s1で次を実行 `erl -name node1@127.0.0.1 --cookie yymmdd`
4. s2で次を実行 `erl -name node2@127.0.0.1 --cookie yymmdd`
5. この時点でs1ではerlangのインタプリンタが起動しているはずである。インタプリンタで次を入力 `sim_sup:init_sim().`
6. s1で次を実行`application:start(brakesim).`
