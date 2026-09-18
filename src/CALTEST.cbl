      *****************************************************************
      * PROGRAM : CALTEST
      * PURPOSE : ATMCAL の単体ドライバ (回帰テスト用)
      * DESIGN  :
      *   曜日区分は実行日に依存するため、ATM 本体を通した検証では
      *   結果が日々変わってしまう。固定日を与えて期待値と突き合わせる
      *   経路をここに分ける。
      *
      *   ケース表には ASCII だけを置き、説明文は EVALUATE で与える。
      *   日本語は UTF-8 で 1 文字 3 バイトになり、OCCURS の固定長に
      *   literal を詰めると桁が合わずコンパイル警告になるため。
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALTEST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CASE-TABLE.
           05  FILLER PIC X(09) VALUE '20260917W'.
           05  FILLER PIC X(09) VALUE '20260919S'.
           05  FILLER PIC X(09) VALUE '20260920H'.
           05  FILLER PIC X(09) VALUE '20260921H'.
           05  FILLER PIC X(09) VALUE '20260922H'.
           05  FILLER PIC X(09) VALUE '20260923H'.
           05  FILLER PIC X(09) VALUE '20260924W'.
           05  FILLER PIC X(09) VALUE '20260503H'.
           05  FILLER PIC X(09) VALUE '20260506H'.
       01  WS-CASES REDEFINES WS-CASE-TABLE.
           05  WS-CASE OCCURS 9 TIMES.
               10  WS-CASE-DATE     PIC 9(08).
               10  WS-CASE-EXPECT   PIC X(01).

       01  WS-CASE-CNT              PIC S9(04) COMP VALUE 9.
       01  WS-I                     PIC S9(04) COMP VALUE ZERO.
       01  WS-VERDICT               PIC X(04) VALUE SPACES.
       01  WS-NOTE                  PIC X(60) VALUE SPACES.
       01  WS-FAILED                PIC 9(04) VALUE ZERO.

       COPY 'CALIF.cpy'.
       COPY 'ATMSESS.cpy'.

       PROCEDURE DIVISION.

       MAIN-CONTROL SECTION.
       MAIN-START.
           MOVE SPACES TO ATM-SESSION

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-CASE-CNT
               PERFORM RUN-ONE-CASE
           END-PERFORM

           PERFORM CHECK-NEXT-BUSINESS
           DISPLAY 'CALTEST 失敗 ' WS-FAILED ' 件'
           STOP RUN.

       RUN-ONE-CASE SECTION.
       ROC-START.
           SET  CAL-FN-DAY-TYPE TO TRUE
           MOVE WS-CASE-DATE(WS-I) TO CAL-IN-DATE
           CALL 'ATMCAL' USING CAL-PARM ATM-SESSION

           IF CAL-OUT-DAY-TYPE = WS-CASE-EXPECT(WS-I)
               MOVE ' OK ' TO WS-VERDICT
           ELSE
               MOVE ' NG ' TO WS-VERDICT
               ADD 1 TO WS-FAILED
           END-IF

           PERFORM SET-NOTE
           DISPLAY WS-VERDICT WS-CASE-DATE(WS-I)
                   ' 区分=' CAL-OUT-DAY-TYPE
                   ' 曜日=' CAL-OUT-DOW
                   ' 祝日=' CAL-OUT-IS-HOLIDAY
                   ' ' FUNCTION TRIM (WS-NOTE).
       ROC-EXIT.
           EXIT.

       SET-NOTE SECTION.
       SN-START.
           EVALUATE WS-I
               WHEN 1 MOVE '木曜 (平日)'             TO WS-NOTE
               WHEN 2 MOVE '土曜'                     TO WS-NOTE
               WHEN 3 MOVE '日曜'                     TO WS-NOTE
               WHEN 4 MOVE '敬老の日 (月曜が祝日)'    TO WS-NOTE
               WHEN 5 MOVE '国民の休日 (火曜が祝日)'  TO WS-NOTE
               WHEN 6 MOVE '秋分の日 (水曜が祝日)'    TO WS-NOTE
               WHEN 7 MOVE '祝日の翌日は平日に戻る'   TO WS-NOTE
               WHEN 8 MOVE '憲法記念日 (日曜と重なる)' TO WS-NOTE
               WHEN 9 MOVE '振替休日'                 TO WS-NOTE
               WHEN OTHER MOVE SPACES                 TO WS-NOTE
           END-EVALUATE.
       SN-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 連休明けの翌営業日。9/19(土) の次は 9/24(木)。
      * 土日に加えて 9/21-23 の 3 連休を飛び越えられるかを見る。
      *----------------------------------------------------------------
       CHECK-NEXT-BUSINESS SECTION.
       CNB-START.
           SET  CAL-FN-NEXT-BUSINESS TO TRUE
           MOVE 20260919 TO CAL-IN-DATE
           CALL 'ATMCAL' USING CAL-PARM ATM-SESSION

           IF CAL-OUT-NEXT-DATE = 20260924
               MOVE ' OK ' TO WS-VERDICT
           ELSE
               MOVE ' NG ' TO WS-VERDICT
               ADD 1 TO WS-FAILED
           END-IF
           DISPLAY WS-VERDICT '20260919 の翌営業日=' CAL-OUT-NEXT-DATE
                   ' (期待 20260924)'.
       CNB-EXIT.
           EXIT.

       END PROGRAM CALTEST.
