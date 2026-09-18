      *****************************************************************
      * PROGRAM : ATMAUTH
      * PURPOSE : カード認証・PIN 検証・カードロック管理
      * DESIGN  :
      *   - PIN は平文で保持しない。カードごとのソルトと連結した値を
      *     ハッシュして比較する。式は CALC-PIN-HASH SECTION が唯一の
      *     所有者で、外部からは HASHPIN 機能で呼ぶ。本番機では HSM の
      *     PIN ブロック検証に置き換わるが、この 1 箇所で完結する。
      *   - 連続失敗 3 回でカードを閉塞 (L)。閉塞済カードの再投入は
      *     即座に拒否し、取込 (C) 指示を上位へ返す。
      *   - 当日累計は「読んだ直後に必ず ROLL-DAILY を通す」ことで
      *     日付跨ぎを 1 箇所で吸収する。日次バッチに依存しない。
      *   - EJECT はカード排出 (ロック解放 + セッション破棄)。
      *     ファイルの CLOSE は端末停止時のみ。
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATMAUTH.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CARD-FILE ASSIGN TO 'data/atmcard.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CARD-PAN
               FILE STATUS IS WS-CARD-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CARD-FILE.
       COPY 'CARDREC.cpy'.

       WORKING-STORAGE SECTION.
       01  WS-CARD-STATUS              PIC X(02) VALUE '00'.
       01  WS-OPENED                   PIC X(01) VALUE 'N'.

       01  WS-CONST.
           05  WS-MAX-PIN-FAIL         PIC 9(01) VALUE 3.
           05  WS-HASH-MULT            PIC 9(07) VALUE 1000003.
           05  WS-HASH-MOD             PIC 9(12) VALUE 999999937.

       01  WS-WORK.
           05  WS-PIN-NUM              PIC 9(04) VALUE ZERO.
           05  WS-SALT                 PIC 9(08) VALUE ZERO.
           05  WS-CALC-HASH            PIC 9(12) VALUE ZERO.
           05  WS-CURRENT-YYYYMM       PIC 9(06) VALUE ZERO.
      *    -- 日付跨ぎクリアの結果をカードへ書き戻すか
           05  WS-PERSIST-DAILY        PIC X(01) VALUE 'N'.

       COPY 'RETCODE.cpy'.

       LINKAGE SECTION.
       COPY 'AUTHIF.cpy'.
       COPY 'ATMSESS.cpy'.

       PROCEDURE DIVISION USING AUTH-PARM ATM-SESSION.

       MAIN-CONTROL SECTION.
       MAIN-START.
           MOVE RC-OK   TO AUTH-OUT-RETCODE
           MOVE EC-NONE TO AUTH-OUT-ERROR-CODE
           MOVE 'N'     TO AUTH-OUT-CARD-CAPTURED

           EVALUATE TRUE
               WHEN AUTH-FN-VERIFY      PERFORM VERIFY-CARD
               WHEN AUTH-FN-CHANGE-PIN  PERFORM CHANGE-PIN
               WHEN AUTH-FN-EJECT       PERFORM CARD-EJECT
               WHEN AUTH-FN-CLOSE       PERFORM CLOSE-CARD-FILE
               WHEN AUTH-FN-LIMIT-CHK   PERFORM CHECK-LIMIT
               WHEN AUTH-FN-ADD-DAILY   PERFORM ADD-DAILY
               WHEN AUTH-FN-HASH-PIN    PERFORM HASH-FOR-CALLER
               WHEN OTHER
                   MOVE RC-FATAL TO AUTH-OUT-RETCODE
           END-EVALUATE
           GOBACK.

       OPEN-CARD-FILE SECTION.
       OPEN-C-START.
           IF WS-OPENED NOT = 'Y'
               OPEN I-O CARD-FILE
               IF WS-CARD-STATUS = '00'
                   MOVE 'Y' TO WS-OPENED
               ELSE
                   MOVE RC-IO-ERROR  TO AUTH-OUT-RETCODE
                   MOVE EC-SYSTEM-IO TO AUTH-OUT-ERROR-CODE
               END-IF
           END-IF.
       OPEN-C-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * VERIFY : カード有効性 → PIN 検証 → 当日累計の日付整合
      *----------------------------------------------------------------
       VERIFY-CARD SECTION.
       VER-START.
           MOVE AUTH-IN-PAN TO SESS-PAN
           PERFORM READ-CARD-LOCKED
           IF AUTH-OUT-RETCODE NOT = RC-OK
               GO TO VER-EXIT
           END-IF

           PERFORM CHECK-CARD-USABLE
           IF AUTH-OUT-RETCODE NOT = RC-OK
               UNLOCK CARD-FILE RECORDS
               GO TO VER-EXIT
           END-IF

           PERFORM VERIFY-PIN-OR-COUNT-UP
           IF AUTH-OUT-RETCODE NOT = RC-OK
               GO TO VER-EXIT
           END-IF

      *    -- 認証成立。失敗カウンタをクリアし、当日累計を整える
           MOVE ZERO TO CARD-PIN-FAIL-CNT
           MOVE SESS-BUSINESS-DATE TO CARD-LAST-USED-DATE
           MOVE 'Y' TO WS-PERSIST-DAILY
           PERFORM ROLL-DAILY
           REWRITE CARD-RECORD
           END-REWRITE
           UNLOCK CARD-FILE RECORDS

           MOVE 'Y'              TO SESS-AUTHENTICATED
           MOVE CARD-ACCT-NO     TO SESS-ACCT-NO
           MOVE CARD-HOLDER-NAME TO SESS-HOLDER-NAME
           PERFORM MASK-PAN.
       VER-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * CHGPIN : 現 PIN 検証済を前提に新 PIN のハッシュを書き換える
      *----------------------------------------------------------------
       CHANGE-PIN SECTION.
       CHG-START.
           PERFORM READ-CARD-LOCKED
           IF AUTH-OUT-RETCODE NOT = RC-OK
               GO TO CHG-EXIT
           END-IF

           PERFORM VERIFY-PIN-OR-COUNT-UP
           IF AUTH-OUT-RETCODE NOT = RC-OK
               GO TO CHG-EXIT
           END-IF

      *    -- ソルトも同時に更新し、旧ハッシュからの逆引きを困難にする
           COMPUTE CARD-PIN-SALT =
               FUNCTION MOD (SESS-TIMESTAMP, 99999989)
           MOVE CARD-PIN-SALT   TO WS-SALT
           MOVE AUTH-IN-NEW-PIN TO WS-PIN-NUM
           PERFORM CALC-PIN-HASH
           MOVE WS-CALC-HASH TO CARD-PIN-HASH
           MOVE ZERO TO CARD-PIN-FAIL-CNT

           REWRITE CARD-RECORD
           END-REWRITE
           UNLOCK CARD-FILE RECORDS.
       CHG-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * LIMITCHK : 1 回あたり / 当日累計金額 / 当日累計回数 の 3 段判定
      *            判定順序は「利用者に伝えるべき理由」の優先度に従う。
      *----------------------------------------------------------------
       CHECK-LIMIT SECTION.
       LIM-START.
           PERFORM OPEN-CARD-FILE
           IF AUTH-OUT-RETCODE NOT = RC-OK
               GO TO LIM-EXIT
           END-IF

           MOVE SESS-PAN TO CARD-PAN
           READ CARD-FILE
               INVALID KEY
                   MOVE RC-NOTFOUND     TO AUTH-OUT-RETCODE
                   MOVE EC-CARD-UNKNOWN TO AUTH-OUT-ERROR-CODE
                   GO TO LIM-EXIT
           END-READ

      *    -- 参照のみなので書き戻さない
           MOVE 'N' TO WS-PERSIST-DAILY
           PERFORM ROLL-DAILY

           EVALUATE TRUE
               WHEN AUTH-IN-AMOUNT > CARD-LIMIT-PER-TXN
                   MOVE RC-BUSINESS-ERROR TO AUTH-OUT-RETCODE
                   MOVE EC-LIMIT-PER-TXN  TO AUTH-OUT-ERROR-CODE
               WHEN CARD-DAILY-WD-AMT + AUTH-IN-AMOUNT
                    > CARD-LIMIT-DAILY-AMT
                   MOVE RC-BUSINESS-ERROR  TO AUTH-OUT-RETCODE
                   MOVE EC-LIMIT-DAILY-AMT TO AUTH-OUT-ERROR-CODE
               WHEN CARD-DAILY-WD-CNT + 1 > CARD-LIMIT-DAILY-CNT
                   MOVE RC-BUSINESS-ERROR  TO AUTH-OUT-RETCODE
                   MOVE EC-LIMIT-DAILY-CNT TO AUTH-OUT-ERROR-CODE
           END-EVALUATE.
       LIM-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * ADDDAILY : 記帳確定後に当日累計へ反映する。マイナス金額を
      *            渡すことで取消 (リバーサル) にも使える。
      *----------------------------------------------------------------
       ADD-DAILY SECTION.
       ADD-START.
           PERFORM READ-CARD-LOCKED
           IF AUTH-OUT-RETCODE NOT = RC-OK
               GO TO ADD-EXIT
           END-IF

           MOVE 'Y' TO WS-PERSIST-DAILY
           PERFORM ROLL-DAILY

           ADD AUTH-IN-AMOUNT TO CARD-DAILY-WD-AMT
           IF AUTH-IN-AMOUNT >= ZERO
               ADD 1 TO CARD-DAILY-WD-CNT
           ELSE
               IF CARD-DAILY-WD-CNT > ZERO
                   SUBTRACT 1 FROM CARD-DAILY-WD-CNT
               END-IF
           END-IF

           REWRITE CARD-RECORD
           END-REWRITE
           UNLOCK CARD-FILE RECORDS.
       ADD-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * HASHPIN : ハッシュ式の外部提供。マスタ作成ユーティリティが
      *           式を複製しなくて済むようにするための機能。
      *----------------------------------------------------------------
       HASH-FOR-CALLER SECTION.
       HFC-START.
           MOVE AUTH-IN-SALT TO WS-SALT
           MOVE AUTH-IN-PIN  TO WS-PIN-NUM
           PERFORM CALC-PIN-HASH
           MOVE WS-CALC-HASH TO AUTH-OUT-HASH.
       HFC-EXIT.
           EXIT.

       CARD-EJECT SECTION.
       EJC-START.
           IF WS-OPENED = 'Y'
               UNLOCK CARD-FILE RECORDS
           END-IF
           MOVE 'N' TO SESS-AUTHENTICATED
           MOVE SPACES TO SESS-PAN SESS-PAN-MASKED
                          SESS-ACCT-NO SESS-HOLDER-NAME.
       EJC-EXIT.
           EXIT.

       CLOSE-CARD-FILE SECTION.
       CLS-START.
           IF WS-OPENED = 'Y'
               UNLOCK CARD-FILE RECORDS
               CLOSE CARD-FILE
               MOVE 'N' TO WS-OPENED
           END-IF.
       CLS-EXIT.
           EXIT.

      *================================================================
      * 共通部品
      *================================================================

      *----------------------------------------------------------------
      * SESS-PAN のカードを排他読みする
      *----------------------------------------------------------------
       READ-CARD-LOCKED SECTION.
       RCL-START.
           PERFORM OPEN-CARD-FILE
           IF AUTH-OUT-RETCODE NOT = RC-OK
               GO TO RCL-EXIT
           END-IF

           MOVE SESS-PAN TO CARD-PAN
           READ CARD-FILE WITH LOCK
               INVALID KEY
                   MOVE RC-NOTFOUND     TO AUTH-OUT-RETCODE
                   MOVE EC-CARD-UNKNOWN TO AUTH-OUT-ERROR-CODE
           END-READ.
       RCL-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * PIN 照合。不一致なら失敗回数を数え、閉塞判定まで行う。
      * 呼出時点でカードはロック済み。抜けるときに必ず解放する。
      *----------------------------------------------------------------
       VERIFY-PIN-OR-COUNT-UP SECTION.
       VPC-START.
           MOVE CARD-PIN-SALT TO WS-SALT
           MOVE AUTH-IN-PIN   TO WS-PIN-NUM
           PERFORM CALC-PIN-HASH
           IF WS-CALC-HASH = CARD-PIN-HASH
               GO TO VPC-EXIT
           END-IF

           ADD 1 TO CARD-PIN-FAIL-CNT
           MOVE RC-BUSINESS-ERROR TO AUTH-OUT-RETCODE
           MOVE EC-PIN-INVALID    TO AUTH-OUT-ERROR-CODE

           IF CARD-PIN-FAIL-CNT >= WS-MAX-PIN-FAIL
               SET CARD-ST-LOCKED TO TRUE
               MOVE EC-CARD-LOCKED TO AUTH-OUT-ERROR-CODE
               MOVE 'Y' TO AUTH-OUT-CARD-CAPTURED
           END-IF

           REWRITE CARD-RECORD
           END-REWRITE
           UNLOCK CARD-FILE RECORDS.
       VPC-EXIT.
           EXIT.

       CHECK-CARD-USABLE SECTION.
       CHK-START.
           EVALUATE TRUE
               WHEN CARD-ST-CAPTURED
                   MOVE RC-BUSINESS-ERROR TO AUTH-OUT-RETCODE
                   MOVE EC-CARD-CAPTURED  TO AUTH-OUT-ERROR-CODE
               WHEN CARD-ST-LOCKED
                   MOVE RC-BUSINESS-ERROR TO AUTH-OUT-RETCODE
                   MOVE EC-CARD-LOCKED    TO AUTH-OUT-ERROR-CODE
                   MOVE 'Y' TO AUTH-OUT-CARD-CAPTURED
           END-EVALUATE
           IF AUTH-OUT-RETCODE NOT = RC-OK
               GO TO CHK-EXIT
           END-IF

      *    -- 有効期限は「当月末まで有効」
           COMPUTE WS-CURRENT-YYYYMM = SESS-BUSINESS-DATE / 100
           IF CARD-EXPIRY-YYYYMM < WS-CURRENT-YYYYMM
               MOVE RC-BUSINESS-ERROR TO AUTH-OUT-RETCODE
               MOVE EC-CARD-EXPIRED   TO AUTH-OUT-ERROR-CODE
           END-IF.
       CHK-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 営業日が変わっていれば当日累計をクリアする。カードレコードを
      * 読んだ直後に必ず通すことで、日跨ぎの扱いを 1 箇所に閉じる。
      * WS-PERSIST-DAILY = 'N' の場合は日付を進めない (参照専用の呼出)。
      *----------------------------------------------------------------
       ROLL-DAILY SECTION.
       ROLL-START.
           IF CARD-DAILY-DATE = SESS-BUSINESS-DATE
               GO TO ROLL-EXIT
           END-IF
           MOVE ZERO TO CARD-DAILY-WD-AMT
           MOVE ZERO TO CARD-DAILY-WD-CNT
           IF WS-PERSIST-DAILY = 'Y'
               MOVE SESS-BUSINESS-DATE TO CARD-DAILY-DATE
           END-IF.
       ROLL-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * PIN ハッシュ。この SECTION が式の唯一の所有者。
      * 本番機では HSM の PIN ブロック検証に置き換わる。
      *
      * 注意: 学習用の簡易式であり暗号学的強度を持たない。
      *----------------------------------------------------------------
       CALC-PIN-HASH SECTION.
       HASH-START.
           COMPUTE WS-CALC-HASH =
               FUNCTION MOD ((WS-SALT + WS-PIN-NUM)
                             * WS-HASH-MULT, WS-HASH-MOD).
       HASH-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * PAN マスク。ジャーナル・画面に平文 PAN を残さないための処理。
      *----------------------------------------------------------------
       MASK-PAN SECTION.
       MSK-START.
           MOVE ALL '*' TO SESS-PAN-MASKED
           MOVE CARD-PAN (13:4) TO SESS-PAN-MASKED (13:4).
       MSK-EXIT.
           EXIT.

       END PROGRAM ATMAUTH.
