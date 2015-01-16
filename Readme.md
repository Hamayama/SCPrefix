# SCPrefix

![image](image.png)

## 概要
- 中置記法の数式をSchemeの前置記法に変換を行うHTMLです。

- 実行例は、以下のページにあります。  
  https://hamayapp.appspot.com/static/scprefix.html


## 使い方
- scprefix.html を ブラウザで開くと起動します。

- srcのテキストボックスに中置記法の数式を貼り付けて、  
  convertボタンをクリックすると、Schemeの前置記法に変換します。  
  結果は、outのテキストボックスに表示されます。  
  (その下にもデバッグ用に同じものが表示されます)

- clearボタンをクリックすると、入出力をすべてクリアします。


## 中置記法の数式の文法
- (各種記号はすべて半角です)

1. 区切り記号  
   半角スペース,タブ,改行が、区切り記号になります。  
   また、セミコロン(;)が、数式の区切り記号になります。  
   (改行のみだと、次の行に式が続く場合があります)

2. コメント  
   /* ～ */ で囲った部分はコメントとなり、変換時に無視されます。  
   また、// を書いた行は行末までコメントとなり、変換時に無視されます。

3. 演算子  
   以下の演算子が使用可能です。  
   レベルが高いほど優先順位が高くなります。  
   (C言語に近いが、微妙に種類や優先順位が異なるので注意)
   ```
     レベル1 : () ! ~ + - ++ -- (括弧,論理NOT,ビットNOT,正符号,負符号,INC,DEC)
     レベル2 : **               (べき乗)
     レベル3 : * / *. /. \ %    (乗算,除算,不正確数乗算,不正確数除算,整数除算,整数剰余)
     レベル4 : + - +. -.        (加算,減算,不正確数加算,不正確数減算)
     レベル5 : << >>            (左ビットシフト,右ビットシフト (正確整数の符号付きシフトのみ))
     レベル6 : < <= > >= == !=  (数値比較 (大小比較は実数のみ)))
     レベル7 : & | ^            (ビットAND,ビットOR,ビットXOR (正確整数のみ))
     レベル8 : && || a?b:c      (論理AND,論理OR,3項演算子)
     レベル9 : , = *= /= *.= /.= += -= +.= -.= \= %= <<= >>= &= |= ^=
                                (カンマ演算子(括弧内でのみ使用可),代入,複合代入)
   ```

4. 名前  
   区切り記号を含まず、演算子以外の文字列は、名前として使用できます。  
   名前は変数名や関数名になります。  
   (現状、数字や文字列リテラル等も、すべて単なる名前として扱われます)

5. 関数  
   名前の後に( )をつけると、関数を表します。  
   ( )内には関数の引数を カンマ(,)で区切って記述します(引数のない関数もあります)。  
   関数は数式中で使用できますが、代入の左辺に置くことはできません。

6. 変数  
   変数は数式中で使用できます。代入の左辺にも置くことができます。


## 注意事項
1. 関数は、f(x1,x2) が (f x1 x2) のように変換されます。  
   実際にSchemeで使用する場合には、関数が定義されている必要があります。

2. 代入(x=1 等)は、set! に変換されます。  
   実際にSchemeで使用する場合には、変数が define 等で定義されている必要があります。

3. INC,DEC(x++ 等)は、inc!, dec! に変換されます。  
   inc!, dec! は処理系依存の命令です(Gaucheに存在します)。

4. ポストINC,DECの値を使う場合(y=x++ 等)は、begin0 を使った式に変換されます。  
   begin0 は処理系依存の命令です(Gaucheに存在します)。

5. 現状、数値や文字列リテラルを認識していないため、エラーチェックが不十分です。  
   例えば、以下が挙げられます。  
   - 数値への代入がエラーにならない (100=1000 等)
   - 数値のINC,DECがエラーにならない (100++ 等)


## 環境等
- 以下の環境で動作を確認しました。
  - OS
    - Windows 8 (64bit)
  - ブラウザ
    - Chrome v39

## 履歴
- 2015-1-16 v1.00 (初版)
- 2015-1-16 v1.01 コメント修正のみ


(2015-1-16)
