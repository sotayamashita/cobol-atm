      *****************************************************************
      * PROGRAM : ATMCAL
      * PURPOSE : 営業日カレンダー (曜日区分・祝日判定・翌営業日)
      * DESIGN  :
      *   曜日は祝日マスタに依存せず FUNCTION INTEGER-OF-DATE から導く。
      *   同関数は 1601-01-01 を 1 とする通日を返し、1601-01-01 は
      *   月曜日なので、通日 mod 7 が 1 なら月曜、0 なら日曜になる。
      *   祝日マスタを引くのは「その日が祝日か」の 1 点だけ。
      *
      *   区分は実務規則に合わせて 3 つに畳む。
      *     W = 平日      S = 土曜      H = 日曜・祝日
      *   平日・土曜が祝日にあたる場合は日曜・休日扱いとする。これは
      *   手数料でも全銀システムの接続時間でも共通の扱いなので、
      *   判定をこのモジュール 1 箇所に閉じ込める。
      *
      *   祝日マスタは起動時に一度だけ読んでテーブルに載せる。判定は
      *   取引のたびに走るため、都度 I/O させない。
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATMCAL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT HOL-FILE ASSIGN TO 'data/atmhol.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-HOL-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  HOL-FILE.
       01  HOL-RECORD.
           05  HOL-DATE                PIC 9(08).
           05  FILLER                  PIC X(32).

       WORKING-STORAGE SECTION.
       01  WS-HOL-STATUS               PIC X(02) VALUE '00'.
       01  WS-LOADED                   PIC X(01) VALUE 'N'.

       01  WS-CONST.
           05  WS-MAX-HOLIDAYS         PIC S9(04) COMP VALUE 400.

      *    -- 祝日テーブル。昇順で保持し、二分探索で引く。
       01  WS-HOLIDAY-TABLE.
           05  WS-HOL-CNT              PIC S9(04) COMP VALUE ZERO.
           05  WS-HOL-ENTRY OCCURS 400 TIMES PIC 9(08).

       01  WS-WORK.
           05  WS-SERIAL               PIC S9(09) COMP VALUE ZERO.
           05  WS-DOW-RAW              PIC S9(04) COMP VALUE ZERO.
           05  WS-I                    PIC S9(04) COMP VALUE ZERO.
           05  WS-LO                   PIC S9(04) COMP VALUE ZERO.
           05  WS-HI                   PIC S9(04) COMP VALUE ZERO.
           05  WS-MID                  PIC S9(04) COMP VALUE ZERO.
           05  WS-FOUND                PIC X(01) VALUE 'N'.
           05  WS-PROBE-DATE           PIC 9(08) VALUE ZERO.
           05  WS-GUARD                PIC S9(04) COMP VALUE ZERO.

       COPY 'RETCODE.cpy'.

       LINKAGE SECTION.
       COPY 'CALIF.cpy'.
       COPY 'ATMSESS.cpy'.

       PROCEDURE DIVISION USING CAL-PARM ATM-SESSION.

       MAIN-CONTROL SECTION.
       MAIN-START.
           MOVE RC-OK TO CAL-OUT-RETCODE

           EVALUATE TRUE
               WHEN CAL-FN-DAY-TYPE       PERFORM CLASSIFY-DATE
               WHEN CAL-FN-NEXT-BUSINESS  PERFORM FIND-NEXT-BUSINESS
               WHEN CAL-FN-CLOSE          CONTINUE
               WHEN OTHER
                   MOVE RC-FATAL TO CAL-OUT-RETCODE
           END-EVALUATE
           GOBACK.

      *----------------------------------------------------------------
      * DAYTYPE : 指定日の区分を返す
      *----------------------------------------------------------------
       CLASSIFY-DATE SECTION.
       CLS-START.
           PERFORM LOAD-HOLIDAYS

           MOVE CAL-IN-DATE TO WS-PROBE-DATE
           PERFORM CLASSIFY-PROBE

           MOVE WS-DOW-RAW TO CAL-OUT-DOW
           MOVE WS-FOUND   TO CAL-OUT-IS-HOLIDAY
           MOVE CAL-IN-DATE TO CAL-OUT-NEXT-DATE.
       CLS-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * WS-PROBE-DATE を区分する。結果は CAL-OUT-DAY-TYPE と
      * WS-DOW-RAW / WS-FOUND に入る。翌営業日探索からも使う。
      *----------------------------------------------------------------
       CLASSIFY-PROBE SECTION.
       CP-START.
           COMPUTE WS-SERIAL =
               FUNCTION INTEGER-OF-DATE (WS-PROBE-DATE)
      *    -- 1601-01-01 は月曜日。mod 7 で 1=月 … 6=土, 0=日
           COMPUTE WS-DOW-RAW = FUNCTION MOD (WS-SERIAL, 7)
           IF WS-DOW-RAW = ZERO
               MOVE 7 TO WS-DOW-RAW
           END-IF

           PERFORM LOOKUP-HOLIDAY

      *    -- 祝日は曜日を問わず休日区分に寄せる。平日・土曜が祝日で
      *    -- あれば日曜・休日扱い、という実務規則をここで表現する。
           EVALUATE TRUE
               WHEN WS-FOUND = 'Y'
                   SET CAL-DT-HOLIDAY  TO TRUE
               WHEN WS-DOW-RAW = 7
                   SET CAL-DT-HOLIDAY  TO TRUE
               WHEN WS-DOW-RAW = 6
                   SET CAL-DT-SATURDAY TO TRUE
               WHEN OTHER
                   SET CAL-DT-WEEKDAY  TO TRUE
           END-EVALUATE.
       CP-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * NEXTBIZ : 指定日の「翌営業日」を返す。土日祝を読み飛ばす。
      *           連休があるため 1 日ずつ進めて判定する。
      *----------------------------------------------------------------
       FIND-NEXT-BUSINESS SECTION.
       FNB-START.
           PERFORM LOAD-HOLIDAYS

           COMPUTE WS-SERIAL =
               FUNCTION INTEGER-OF-DATE (CAL-IN-DATE)
           MOVE ZERO TO WS-GUARD

      *    -- 最大 30 日で打ち切る。祝日マスタの入れ違い等で無限に
      *    -- 進み続けることを防ぐための保険。
           PERFORM UNTIL WS-GUARD > 30
               ADD 1 TO WS-SERIAL
               ADD 1 TO WS-GUARD
               COMPUTE WS-PROBE-DATE =
                   FUNCTION DATE-OF-INTEGER (WS-SERIAL)
               PERFORM CLASSIFY-PROBE
               IF CAL-DT-WEEKDAY
                   MOVE WS-PROBE-DATE TO CAL-OUT-NEXT-DATE
                   MOVE WS-DOW-RAW    TO CAL-OUT-DOW
                   MOVE WS-FOUND      TO CAL-OUT-IS-HOLIDAY
                   GO TO FNB-EXIT
               END-IF
           END-PERFORM

           MOVE RC-FATAL TO CAL-OUT-RETCODE.
       FNB-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 祝日マスタの読込。起動後 1 回だけ。
      * マスタが無くても動くようにする (土日判定だけは成立するため)。
      *----------------------------------------------------------------
       LOAD-HOLIDAYS SECTION.
       LOAD-START.
           IF WS-LOADED = 'Y'
               GO TO LOAD-EXIT
           END-IF
           MOVE 'Y'  TO WS-LOADED
           MOVE ZERO TO WS-HOL-CNT

           OPEN INPUT HOL-FILE
           IF WS-HOL-STATUS NOT = '00' AND WS-HOL-STATUS NOT = '05'
               GO TO LOAD-EXIT
           END-IF

           PERFORM UNTIL WS-HOL-STATUS NOT = '00'
               READ HOL-FILE
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF WS-HOL-CNT < WS-MAX-HOLIDAYS
                           ADD 1 TO WS-HOL-CNT
                           MOVE HOL-DATE TO WS-HOL-ENTRY(WS-HOL-CNT)
                       END-IF
               END-READ
           END-PERFORM
           CLOSE HOL-FILE.
       LOAD-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 祝日テーブルの二分探索。マスタは昇順である前提。
      *----------------------------------------------------------------
       LOOKUP-HOLIDAY SECTION.
       LOOK-START.
           MOVE 'N' TO WS-FOUND
           IF WS-HOL-CNT = ZERO
               GO TO LOOK-EXIT
           END-IF

           MOVE 1          TO WS-LO
           MOVE WS-HOL-CNT TO WS-HI
           PERFORM UNTIL WS-LO > WS-HI
               COMPUTE WS-MID = (WS-LO + WS-HI) / 2
               EVALUATE TRUE
                   WHEN WS-HOL-ENTRY(WS-MID) = WS-PROBE-DATE
                       MOVE 'Y' TO WS-FOUND
                       EXIT PERFORM
                   WHEN WS-HOL-ENTRY(WS-MID) < WS-PROBE-DATE
                       COMPUTE WS-LO = WS-MID + 1
                   WHEN OTHER
                       COMPUTE WS-HI = WS-MID - 1
               END-EVALUATE
           END-PERFORM.
       LOOK-EXIT.
           EXIT.

       END PROGRAM ATMCAL.
