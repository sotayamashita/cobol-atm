      *****************************************************************
      * PROGRAM : ATMZGN
      * PURPOSE : 全銀システム (内国為替) 接続の模擬
      * DESIGN  :
      *   全銀システムは 2 階建て。
      *     コアタイム : 平日 8:30-15:30。加盟行は接続が義務であり、
      *                  相手行ごとの参加可否を判定する必要はない。
      *     モアタイム : 平日夜間・土日祝を処理する。参加は任意で、
      *                  参加行でも接続時間を限定する行が残っている。
      *   したがって即時着金の可否は「時間帯」だけでは決まらず、
      *   コアタイム外では相手行マスタの参加区分と接続時間まで見る。
      *   どちらにも乗らない場合は翌営業日入金となる。
      *
      *   曜日区分と翌営業日は ATMCAL に委譲する。祝日マスタを持つのは
      *   ATMCAL だけであり、ここで曜日を再計算すると祝日の扱いが
      *   二重定義になって手数料判定と食い違うため。
      *
      *   ROUTE は通信を伴わない純粋な判定。SEND / CANCEL のみが
      *   電文送信を模擬する。記帳前に経路を確定できるよう分けている。
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATMZGN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BANK-FILE ASSIGN TO 'data/atmbank.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BANK-CD
               FILE STATUS IS WS-BANK-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  BANK-FILE.
       COPY 'BANKREC.cpy'.

       WORKING-STORAGE SECTION.
       01  WS-BANK-STATUS              PIC X(02) VALUE '00'.
       01  WS-OPENED                   PIC X(01) VALUE 'N'.

      *    -- 追跡番号の連番。取消電文と突き合わせるため、電文ごとに
      *    -- 必ず新しい番号を採番する。
       01  WS-TRACE-SEQ                PIC 9(06) VALUE ZERO.

      *    -- タイムアウトを再現させるための境界値。乱数で落とすと
      *    -- 試験が再現できないため、金額による決定的な条件にする。
       01  WS-TIMEOUT-TRIGGER          PIC 9(04) VALUE 9999.

       01  WS-WORK.
           05  WS-TS                   PIC 9(14) VALUE ZERO.
           05  WS-TS-R REDEFINES WS-TS.
               10  WS-TS-DATE          PIC 9(08).
               10  WS-TS-HHMM          PIC 9(04).
               10  WS-TS-SS            PIC 9(02).
           05  WS-AMT-INT              PIC 9(13) VALUE ZERO.
           05  WS-AMT-LOW              PIC 9(04) VALUE ZERO.
           05  WS-MT-OK                PIC X(01) VALUE 'N'.

       01  WS-CORE-TIME.
           05  WS-CORE-FROM            PIC 9(04) VALUE 0830.
           05  WS-CORE-TO              PIC 9(04) VALUE 1530.

       COPY 'CALIF.cpy'.
       COPY 'RETCODE.cpy'.

       LINKAGE SECTION.
       COPY 'ZGNIF.cpy'.
       COPY 'ATMSESS.cpy'.

       PROCEDURE DIVISION USING ZGN-PARM ATM-SESSION.

       MAIN-CONTROL SECTION.
       MAIN-START.
           MOVE RC-OK   TO ZGN-OUT-RETCODE
           MOVE EC-NONE TO ZGN-OUT-ERROR-CODE

           EVALUATE TRUE
               WHEN ZGN-FN-ROUTE   PERFORM DECIDE-ROUTE
               WHEN ZGN-FN-SEND    PERFORM SEND-MESSAGE
               WHEN ZGN-FN-CANCEL  PERFORM CANCEL-MESSAGE
               WHEN ZGN-FN-CLOSE   PERFORM CLOSE-BANK
               WHEN OTHER
                   MOVE RC-FATAL TO ZGN-OUT-RETCODE
           END-EVALUATE
           GOBACK.

      *----------------------------------------------------------------
      * ROUTE : 相手行と現在時刻から経路と入金日を決める。通信しない。
      *----------------------------------------------------------------
       DECIDE-ROUTE SECTION.
       RT-START.
           MOVE SPACES TO ZGN-OUT-BANK-NAME
           MOVE SPACE  TO ZGN-OUT-ROUTE
           MOVE 'N'    TO ZGN-OUT-IMMEDIATE
           MOVE ZERO   TO ZGN-OUT-VALUE-DATE

           PERFORM LOAD-BANK
           IF ZGN-OUT-RETCODE NOT = RC-OK
               GO TO RT-EXIT
           END-IF

           MOVE BANK-NAME TO ZGN-OUT-BANK-NAME

      *    -- 障害等で相手行が接続を落としている間は、時間帯を問わず
      *    -- 電文を出せない。翌営業日扱いにもできないので拒否する。
           IF NOT BANK-IS-ONLINE
               MOVE RC-BUSINESS-ERROR  TO ZGN-OUT-RETCODE
               MOVE EC-BANK-OFFLINE    TO ZGN-OUT-ERROR-CODE
               GO TO RT-EXIT
           END-IF

           MOVE SESS-TIMESTAMP TO WS-TS
           PERFORM GET-DAY-TYPE
           IF ZGN-OUT-RETCODE NOT = RC-OK
               GO TO RT-EXIT
           END-IF

           IF CAL-DT-WEEKDAY
              AND WS-TS-HHMM >= WS-CORE-FROM
              AND WS-TS-HHMM <= WS-CORE-TO
               SET ZGN-RT-CORE TO TRUE
               MOVE 'Y'        TO ZGN-OUT-IMMEDIATE
               MOVE WS-TS-DATE TO ZGN-OUT-VALUE-DATE
               GO TO RT-EXIT
           END-IF

           PERFORM CHECK-MORETIME
           IF WS-MT-OK = 'Y'
               SET ZGN-RT-MORETIME TO TRUE
               MOVE 'Y'            TO ZGN-OUT-IMMEDIATE
               MOVE WS-TS-DATE     TO ZGN-OUT-VALUE-DATE
               GO TO RT-EXIT
           END-IF

      *    -- 相手行がモアタイム未参加、または接続時間外。翌営業日の
      *    -- 入金になることは失敗ではないので、利用者に予告できるよう
      *    -- 業務エラーコードだけ立てて経路は返す。
           PERFORM GET-NEXT-BUSINESS
           IF ZGN-OUT-RETCODE NOT = RC-OK
               GO TO RT-EXIT
           END-IF
           SET ZGN-RT-NEXT-DAY TO TRUE
           MOVE 'N'                 TO ZGN-OUT-IMMEDIATE
           MOVE CAL-OUT-NEXT-DATE   TO ZGN-OUT-VALUE-DATE
           MOVE RC-BUSINESS-ERROR   TO ZGN-OUT-RETCODE
           MOVE EC-NEXT-BUSINESS-DAY TO ZGN-OUT-ERROR-CODE.
       RT-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 相手行のモアタイム接続時間内かを判定する。
      *   FROM = TO は 24 時間接続 (BANKREC.cpy の規約)。
      *   FROM > TO は日をまたぐ窓 (例 1800-0800) として扱う。
      *----------------------------------------------------------------
       CHECK-MORETIME SECTION.
       MT-START.
           MOVE 'N' TO WS-MT-OK
           IF NOT BANK-MT-JOINED
               GO TO MT-EXIT
           END-IF

           EVALUATE TRUE
               WHEN BANK-MT-FROM-HHMM = BANK-MT-TO-HHMM
                   MOVE 'Y' TO WS-MT-OK
               WHEN BANK-MT-FROM-HHMM < BANK-MT-TO-HHMM
                   IF WS-TS-HHMM >= BANK-MT-FROM-HHMM
                      AND WS-TS-HHMM <= BANK-MT-TO-HHMM
                       MOVE 'Y' TO WS-MT-OK
                   END-IF
               WHEN OTHER
                   IF WS-TS-HHMM >= BANK-MT-FROM-HHMM
                      OR WS-TS-HHMM <= BANK-MT-TO-HHMM
                       MOVE 'Y' TO WS-MT-OK
                   END-IF
           END-EVALUATE.
       MT-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * SEND : 為替電文の送信を模擬する。
      *   ここで返すタイムアウトは「失敗」ではなく「成否不明」である。
      *   相手行が受電済みで応答だけ落ちた可能性があるため、呼出元は
      *     (1) 同じ追跡番号で CANCEL を送って取り消すか、
      *     (2) 不確定取引として EJ に残し、日次で相手行と突合する
      *   のいずれかを必ず行うこと。単純な再送は二重入金になる。
      *   そのため追跡番号はタイムアウト時にも必ず返す。
      *
      *   発生条件は金額の下 4 桁が 9999 円のとき。乱数を使うと試験が
      *   再現できないので、決定的な条件にしてある。
      *----------------------------------------------------------------
       SEND-MESSAGE SECTION.
       SND-START.
           PERFORM MAKE-TRACE-NO

           COMPUTE WS-AMT-INT = FUNCTION INTEGER (ZGN-IN-AMOUNT)
           COMPUTE WS-AMT-LOW = FUNCTION MOD (WS-AMT-INT, 10000)
           IF WS-AMT-LOW = WS-TIMEOUT-TRIGGER
               MOVE RC-IO-ERROR        TO ZGN-OUT-RETCODE
               MOVE EC-ZENGIN-TIMEOUT  TO ZGN-OUT-ERROR-CODE
           END-IF.
       SND-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * CANCEL : SEND と同じ追跡番号で取消電文を送る。
      *   番号を採番し直すと相手行が別取引とみなして取り消せないため、
      *   呼出元が持っている ZGN-OUT-TRACE-NO をそのまま使う。
      *----------------------------------------------------------------
       CANCEL-MESSAGE SECTION.
       CAN-START.
           IF ZGN-OUT-TRACE-NO = SPACES
               MOVE RC-FATAL TO ZGN-OUT-RETCODE
           END-IF.
       CAN-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 追跡番号 = 発信時刻 HHMMSS + 6 桁連番。同一秒の連続送信でも
      * 重複しないようにする。
      *----------------------------------------------------------------
       MAKE-TRACE-NO SECTION.
       TRC-START.
           MOVE SESS-TIMESTAMP TO WS-TS
           ADD 1 TO WS-TRACE-SEQ
           MOVE SPACES TO ZGN-OUT-TRACE-NO
           STRING WS-TS-HHMM    DELIMITED BY SIZE
                  WS-TS-SS      DELIMITED BY SIZE
                  WS-TRACE-SEQ  DELIMITED BY SIZE
               INTO ZGN-OUT-TRACE-NO
           END-STRING.
       TRC-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 金融機関マスタの読込
      *----------------------------------------------------------------
       LOAD-BANK SECTION.
       LB-START.
           IF WS-OPENED NOT = 'Y'
               PERFORM OPEN-BANK
               IF ZGN-OUT-RETCODE NOT = RC-OK
                   GO TO LB-EXIT
               END-IF
           END-IF

           MOVE ZGN-IN-BANK-CD TO BANK-CD
           READ BANK-FILE
               INVALID KEY
                   MOVE RC-BUSINESS-ERROR TO ZGN-OUT-RETCODE
                   MOVE EC-BANK-UNKNOWN   TO ZGN-OUT-ERROR-CODE
           END-READ.
       LB-EXIT.
           EXIT.

       OPEN-BANK SECTION.
       OB-START.
           OPEN INPUT BANK-FILE
           IF WS-BANK-STATUS = '00'
               MOVE 'Y' TO WS-OPENED
           ELSE
               MOVE RC-IO-ERROR  TO ZGN-OUT-RETCODE
               MOVE EC-SYSTEM-IO TO ZGN-OUT-ERROR-CODE
           END-IF.
       OB-EXIT.
           EXIT.

       CLOSE-BANK SECTION.
       CB-START.
           IF WS-OPENED = 'Y'
               CLOSE BANK-FILE
               MOVE 'N' TO WS-OPENED
           END-IF.
       CB-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * ATMCAL 委譲部
      *----------------------------------------------------------------
       GET-DAY-TYPE SECTION.
       GDT-START.
           SET CAL-FN-DAY-TYPE TO TRUE
           MOVE WS-TS-DATE TO CAL-IN-DATE
           CALL 'ATMCAL' USING CAL-PARM ATM-SESSION
           IF CAL-OUT-RETCODE NOT = RC-OK
               MOVE RC-FATAL     TO ZGN-OUT-RETCODE
               MOVE EC-SYSTEM-IO TO ZGN-OUT-ERROR-CODE
           END-IF.
       GDT-EXIT.
           EXIT.

       GET-NEXT-BUSINESS SECTION.
       GNB-START.
           SET CAL-FN-NEXT-BUSINESS TO TRUE
           MOVE WS-TS-DATE TO CAL-IN-DATE
           CALL 'ATMCAL' USING CAL-PARM ATM-SESSION
           IF CAL-OUT-RETCODE NOT = RC-OK
               MOVE RC-FATAL     TO ZGN-OUT-RETCODE
               MOVE EC-SYSTEM-IO TO ZGN-OUT-ERROR-CODE
           END-IF.
       GNB-EXIT.
           EXIT.

       END PROGRAM ATMZGN.
