      *****************************************************************
      * PROGRAM : ZGNTEST
      * PURPOSE : ATMZGN の単体ドライバ (回帰テスト用)
      * DESIGN  :
      *   経路判定は「日時 × 相手行のモアタイム参加状況」で決まる。
      *   実行時刻に依存させると検証にならないため、SESS-TIMESTAMP を
      *   固定値で与えて期待値と突き合わせる。
      *
      *   金融機関マスタ (ATMSEED が生成):
      *     0001 自行              0005 モアタイム参加 (24 時間)
      *     0009 モアタイム未参加   0012 参加だが 08:00-21:00 限定
      *
      *   ケース表には ASCII だけを置き、説明文は EVALUATE で与える。
      *   日本語を OCCURS の固定長に詰めるとバイト数が合わないため。
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZGNTEST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CASE-TABLE.
           05  FILLER PIC X(19) VALUE '000520260917103000C'.
           05  FILLER PIC X(19) VALUE '000520260917200000M'.
           05  FILLER PIC X(19) VALUE '000920260917200000N'.
           05  FILLER PIC X(19) VALUE '000920260917103000C'.
           05  FILLER PIC X(19) VALUE '001220260917200000M'.
           05  FILLER PIC X(19) VALUE '001220260917223000N'.
           05  FILLER PIC X(19) VALUE '000520260920103000M'.
           05  FILLER PIC X(19) VALUE '000920260920103000N'.
           05  FILLER PIC X(19) VALUE '000920260921103000N'.
       01  WS-CASES REDEFINES WS-CASE-TABLE.
           05  WS-CASE OCCURS 9 TIMES.
               10  WS-CASE-BANK     PIC X(04).
               10  WS-CASE-TS       PIC 9(14).
               10  WS-CASE-EXPECT   PIC X(01).

       01  WS-CASE-CNT              PIC S9(04) COMP VALUE 9.
       01  WS-I                     PIC S9(04) COMP VALUE ZERO.
       01  WS-VERDICT               PIC X(04) VALUE SPACES.
       01  WS-NOTE                  PIC X(60) VALUE SPACES.
       01  WS-FAILED                PIC 9(04) VALUE ZERO.

       COPY 'ZGNIF.cpy'.
       COPY 'CALIF.cpy'.
       COPY 'ATMSESS.cpy'.

       PROCEDURE DIVISION.

       MAIN-CONTROL SECTION.
       MAIN-START.
           MOVE SPACES TO ATM-SESSION

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-CASE-CNT
               PERFORM RUN-ONE-CASE
           END-PERFORM

           PERFORM CHECK-TIMEOUT

           SET ZGN-FN-CLOSE TO TRUE
           CALL 'ATMZGN' USING ZGN-PARM ATM-SESSION
           DISPLAY 'ZGNTEST 失敗 ' WS-FAILED ' 件'
           STOP RUN.

       RUN-ONE-CASE SECTION.
       ROC-START.
           MOVE WS-CASE-TS(WS-I) TO SESS-TIMESTAMP
           COMPUTE SESS-BUSINESS-DATE = WS-CASE-TS(WS-I) / 1000000
      *    -- 曜日区分は本来 ATMMAIN が取引開始時に確定させる。
      *    -- 本体を経由しないドライバなので、ここで同じ手順を踏む。
           PERFORM RESOLVE-DAY-TYPE

           SET  ZGN-FN-ROUTE TO TRUE
           MOVE WS-CASE-BANK(WS-I) TO ZGN-IN-BANK-CD
           MOVE '9999999999'       TO ZGN-IN-ACCT-NO
           MOVE 30000.00           TO ZGN-IN-AMOUNT
           CALL 'ATMZGN' USING ZGN-PARM ATM-SESSION

           IF ZGN-OUT-ROUTE = WS-CASE-EXPECT(WS-I)
               MOVE ' OK ' TO WS-VERDICT
           ELSE
               MOVE ' NG ' TO WS-VERDICT
               ADD 1 TO WS-FAILED
           END-IF

           PERFORM SET-NOTE
           DISPLAY WS-VERDICT WS-CASE-BANK(WS-I)
                   ' ' WS-CASE-TS(WS-I)
                   ' 経路=' ZGN-OUT-ROUTE
                   ' 入金日=' ZGN-OUT-VALUE-DATE
                   ' ' FUNCTION TRIM (WS-NOTE).
       ROC-EXIT.
           EXIT.

       RESOLVE-DAY-TYPE SECTION.
       RDT-START.
           SET  CAL-FN-DAY-TYPE TO TRUE
           MOVE SESS-BUSINESS-DATE TO CAL-IN-DATE
           CALL 'ATMCAL' USING CAL-PARM ATM-SESSION
           MOVE CAL-OUT-DAY-TYPE TO SESS-DAY-TYPE.
       RDT-EXIT.
           EXIT.

       SET-NOTE SECTION.
       SN-START.
           EVALUATE WS-I
               WHEN 1 MOVE '平日日中はコアタイム'        TO WS-NOTE
               WHEN 2 MOVE '平日夜間で相手行が参加'      TO WS-NOTE
               WHEN 3 MOVE '平日夜間で相手行が未参加'    TO WS-NOTE
               WHEN 4 MOVE '未参加行でもコアタイム中は即時'
                                                         TO WS-NOTE
               WHEN 5 MOVE '時間限定参加の接続時間内'    TO WS-NOTE
               WHEN 6 MOVE '時間限定参加の接続時間外'    TO WS-NOTE
               WHEN 7 MOVE '日曜はコアタイム停止'        TO WS-NOTE
               WHEN 8 MOVE '日曜 + 未参加行は翌営業日'   TO WS-NOTE
               WHEN 9 MOVE '祝日 + 未参加行は翌営業日'   TO WS-NOTE
               WHEN OTHER MOVE SPACES                    TO WS-NOTE
           END-EVALUATE.
       SN-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * タイムアウトは「失敗」ではなく「成否不明」。単純再送は二重入金に
      * なるため、呼出元は取消電文を送るか不確定取引として EJ に残す。
      *----------------------------------------------------------------
       CHECK-TIMEOUT SECTION.
       CT-START.
           MOVE 20260917103000 TO SESS-TIMESTAMP
           MOVE 20260917       TO SESS-BUSINESS-DATE
           PERFORM RESOLVE-DAY-TYPE
           SET  ZGN-FN-SEND TO TRUE
           MOVE '0005'         TO ZGN-IN-BANK-CD
           MOVE 39999.00       TO ZGN-IN-AMOUNT
           CALL 'ATMZGN' USING ZGN-PARM ATM-SESSION

           IF ZGN-OUT-ERROR-CODE = '5003'
               MOVE ' OK ' TO WS-VERDICT
           ELSE
               MOVE ' NG ' TO WS-VERDICT
               ADD 1 TO WS-FAILED
           END-IF
           DISPLAY WS-VERDICT 'SEND 39,999 円 エラー='
                   ZGN-OUT-ERROR-CODE ' (期待 5003)'.
       CT-EXIT.
           EXIT.

       END PROGRAM ZGNTEST.
