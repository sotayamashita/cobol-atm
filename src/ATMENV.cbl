      *****************************************************************
      * PROGRAM : ATMENV
      * PURPOSE : 端末 ID の解決
      * DESIGN  :
      *   端末 ID は実行時に環境変数 ATM_ID で与える。与えられなければ
      *   ATMCONST.cpy の既定値を使う。
      *
      *   解決をモジュール 1 つに閉じるのは、ATMCONST.cpy が「値の
      *   所有者を 1 箇所にする」と述べているのと同じ理由による。
      *   端末 ID は現金カセット・EJ・締め状態のファイル名を決めるので、
      *   プログラムごとに解決規則が食い違うと、同じ端末のつもりで
      *   別々のファイルを掴む。起動時に現金カセットが引けず全取引が
      *   止まる、という ATMCONST.cpy の警告がそのまま当てはまる。
      *
      *   定数ではなくモジュールにしたのは、値が実行時に決まるため。
      *   コピー句はデータ定義しか持てず、解決の手続きを共有できない。
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATMENV.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ENV-VALUE                PIC X(08) VALUE SPACES.

       COPY 'ATMCONST.cpy'.

       LINKAGE SECTION.
       01  LK-ATM-ID                   PIC X(08).

       PROCEDURE DIVISION USING LK-ATM-ID.

       MAIN-CONTROL SECTION.
       MAIN-START.
           MOVE SPACES TO WS-ENV-VALUE
           ACCEPT WS-ENV-VALUE FROM ENVIRONMENT 'ATM_ID'
               ON EXCEPTION
                   MOVE SPACES TO WS-ENV-VALUE
           END-ACCEPT

           IF WS-ENV-VALUE = SPACES
               MOVE CN-ATM-ID TO LK-ATM-ID
           ELSE
               MOVE WS-ENV-VALUE TO LK-ATM-ID
           END-IF
           GOBACK.

       END PROGRAM ATMENV.
