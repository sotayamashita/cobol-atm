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
      *   - 媒体は磁気 (JIS II 型) と IC (全銀協 IC キャッシュカード
      *     標準仕様 / EMV 準拠) が併存する。PIN の照合場所が違い、
      *     磁気はホスト照合、IC はカード内照合 (オフライン PIN) も
      *     可能なので、認証方式を確定してセッションに残す。
      *   - 限度額は「媒体 × 認証方式」で変わるため限度額マスタを
      *     持つ。取引のたびに引く値なので起動時に一度だけ全件読む。
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

           SELECT LIMIT-FILE ASSIGN TO 'data/atmlimit.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LIMIT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CARD-FILE.
       COPY 'CARDREC.cpy'.

       FD  LIMIT-FILE.
       COPY 'LIMITREC.cpy'.

       WORKING-STORAGE SECTION.
       01  WS-CARD-STATUS              PIC X(02) VALUE '00'.
       01  WS-LIMIT-STATUS             PIC X(02) VALUE '00'.
       01  WS-OPENED                   PIC X(01) VALUE 'N'.
       01  WS-LIMIT-LOADED             PIC X(01) VALUE 'N'.

       01  WS-CONST.
           05  WS-MAX-PIN-FAIL         PIC 9(01) VALUE 3.
           05  WS-HASH-MULT            PIC 9(07) VALUE 1000003.
           05  WS-HASH-MOD             PIC 9(12) VALUE 999999937.
      *    -- 媒体 (2) × 認証方式 (3) の組合せに余裕を持たせた上限
           05  WS-MAX-LIMITS           PIC S9(04) COMP VALUE 16.

      *    -- 限度額テーブル。キーは 媒体 + 認証方式。件数が高々十数件
      *    -- なので順次探索で足りる。
       01  WS-LIMIT-TABLE.
           05  WS-LIM-CNT              PIC S9(04) COMP VALUE ZERO.
           05  WS-LIM-ENTRY OCCURS 16 TIMES.
               10  WS-LIM-MEDIA        PIC X(01).
               10  WS-LIM-AUTH         PIC X(01).
               10  WS-LIM-PER-TXN      PIC 9(11).
               10  WS-LIM-DAILY-AMT    PIC 9(11).
               10  WS-LIM-DAILY-CNT    PIC 9(03).

       01  WS-WORK.
           05  WS-PIN-NUM              PIC 9(04) VALUE ZERO.
           05  WS-SALT                 PIC 9(08) VALUE ZERO.
           05  WS-CALC-HASH            PIC 9(12) VALUE ZERO.
           05  WS-CURRENT-YYYYMM       PIC 9(06) VALUE ZERO.
      *    -- 日付跨ぎクリアの結果をカードへ書き戻すか
           05  WS-PERSIST-DAILY        PIC X(01) VALUE 'N'.
           05  WS-I                    PIC S9(04) COMP VALUE ZERO.
           05  WS-LIM-IDX              PIC S9(04) COMP VALUE ZERO.
      *    -- 実効限度額 (マスタとカード個別設定の小さい方)
           05  WS-EFF-PER-TXN          PIC S9(13)V99 VALUE ZERO.
           05  WS-EFF-DAILY-AMT        PIC S9(13)V99 VALUE ZERO.
           05  WS-EFF-DAILY-CNT        PIC 9(03) VALUE ZERO.

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

           PERFORM LOAD-LIMITS

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

      *    -- 認証方式は PIN 照合の前に決める。照合の経路そのものが
      *    -- 媒体で変わる (磁気はホスト照合、IC はカード内照合) ため。
           PERFORM DETERMINE-AUTH-METHOD
           IF AUTH-OUT-RETCODE NOT = RC-OK
               UNLOCK CARD-FILE RECORDS
               GO TO VER-EXIT
           END-IF

           PERFORM VERIFY-PIN-OR-COUNT-UP
           IF AUTH-OUT-RETCODE NOT = RC-OK
               GO TO VER-EXIT
           END-IF

      *    -- 生体認証は PIN の代替ではなく上乗せ。国内の IC + 生体
      *    -- 認証 ATM も暗証番号と併用する運用が前提。
           IF SESS-AM-BIOMETRIC
               PERFORM VERIFY-BIOMETRIC
               IF AUTH-OUT-RETCODE NOT = RC-OK
                   UNLOCK CARD-FILE RECORDS
                   GO TO VER-EXIT
               END-IF
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
      * 媒体と認証方式の確定。ここで決めた値は限度額の判定まで効く。
      *   磁気            → P (暗証番号のみ / ホスト照合)
      *   IC 生体未登録   → O (IC オフライン PIN)
      *   IC 生体登録済   → B (IC + 生体認証)
      *----------------------------------------------------------------
       DETERMINE-AUTH-METHOD SECTION.
       DAM-START.
      *    -- 発行区分は手数料体系を決める。カードマスタを読めるのは
      *    -- このモジュールだけなので、ここでセッションへ載せる。
           MOVE CARD-KIND TO SESS-CARD-KIND

           EVALUATE TRUE
               WHEN CARD-MD-MAGNETIC
                   SET SESS-MEDIA-MAGNETIC TO TRUE
                   SET SESS-AM-PIN-ONLY    TO TRUE
               WHEN CARD-MD-IC
                   SET SESS-MEDIA-IC TO TRUE
                   IF CARD-BIO-YES
                       SET SESS-AM-BIOMETRIC TO TRUE
                   ELSE
                       SET SESS-AM-IC-OFFLINE TO TRUE
                   END-IF
               WHEN OTHER
      *            -- 未知の媒体を既定値に寄せると、実際より高い限度額
      *            -- を与えてしまう恐れがあるので取引を成立させない。
                   MOVE RC-BUSINESS-ERROR    TO AUTH-OUT-RETCODE
                   MOVE EC-MEDIA-UNSUPPORTED TO AUTH-OUT-ERROR-CODE
           END-EVALUATE.
       DAM-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 生体認証。本来は静脈・顔などの生体情報を、IC カード内の登録
      * テンプレート (カード内照合) または装置側で照合する。本実装は
      * 読取装置が無いため照合成功を仮定した模擬であり、登録有無の
      * 確認だけを実際に行う。
      *----------------------------------------------------------------
       VERIFY-BIOMETRIC SECTION.
       VBI-START.
           CONTINUE.
       VBI-EXIT.
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
      *
      * 実効限度額は「限度額マスタ (媒体 × 認証方式)」と「カード個別
      * 設定 (CARD-LIMIT-*)」の小さい方を採る。カード個別設定は
      * 「銀行が定める上限の範囲内で顧客が自分の限度額を下げられる」
      * という位置づけであり、顧客設定が銀行上限を超えて効くことは
      * ないため。逆に、暗証番号のみの取引の上限引下げのような行政
      * 指導由来の改定は、マスタ側を直せば全カードに即時反映される。
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

           PERFORM RESOLVE-EFFECTIVE-LIMIT
           IF AUTH-OUT-RETCODE NOT = RC-OK
               GO TO LIM-EXIT
           END-IF

           EVALUATE TRUE
               WHEN AUTH-IN-AMOUNT > WS-EFF-PER-TXN
                   MOVE RC-BUSINESS-ERROR TO AUTH-OUT-RETCODE
                   MOVE EC-LIMIT-PER-TXN  TO AUTH-OUT-ERROR-CODE
               WHEN CARD-DAILY-WD-AMT + AUTH-IN-AMOUNT
                    > WS-EFF-DAILY-AMT
                   MOVE RC-BUSINESS-ERROR  TO AUTH-OUT-RETCODE
                   MOVE EC-LIMIT-DAILY-AMT TO AUTH-OUT-ERROR-CODE
               WHEN CARD-DAILY-WD-CNT + 1 > WS-EFF-DAILY-CNT
                   MOVE RC-BUSINESS-ERROR  TO AUTH-OUT-RETCODE
                   MOVE EC-LIMIT-DAILY-CNT TO AUTH-OUT-ERROR-CODE
           END-EVALUATE.
       LIM-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 実効限度額の決定。マスタ値とカード個別設定の小さい方を採る。
      * 呼出時点でカードレコードは読込済みであること。
      *----------------------------------------------------------------
       RESOLVE-EFFECTIVE-LIMIT SECTION.
       REL-START.
           PERFORM LOOKUP-LIMIT
           IF WS-LIM-IDX = ZERO
      *        -- 媒体・認証方式に対応する行が無い。既定値で通すと
      *        -- 上限を誤って広げるため、取引を成立させない。
      *        -- 原因はカードの性質ではなくマスタの整備漏れなので、
      *        -- 係員が EJ で区別できるよう 1006 とは別コードにする。
               MOVE RC-BUSINESS-ERROR TO AUTH-OUT-RETCODE
               MOVE EC-PARM-MISSING   TO AUTH-OUT-ERROR-CODE
               GO TO REL-EXIT
           END-IF

           MOVE WS-LIM-PER-TXN   (WS-LIM-IDX) TO WS-EFF-PER-TXN
           MOVE WS-LIM-DAILY-AMT (WS-LIM-IDX) TO WS-EFF-DAILY-AMT
           MOVE WS-LIM-DAILY-CNT (WS-LIM-IDX) TO WS-EFF-DAILY-CNT

           IF CARD-LIMIT-PER-TXN < WS-EFF-PER-TXN
               MOVE CARD-LIMIT-PER-TXN TO WS-EFF-PER-TXN
           END-IF
           IF CARD-LIMIT-DAILY-AMT < WS-EFF-DAILY-AMT
               MOVE CARD-LIMIT-DAILY-AMT TO WS-EFF-DAILY-AMT
           END-IF
           IF CARD-LIMIT-DAILY-CNT < WS-EFF-DAILY-CNT
               MOVE CARD-LIMIT-DAILY-CNT TO WS-EFF-DAILY-CNT
           END-IF.
       REL-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 限度額テーブルの検索。キーは 媒体 + 認証方式。
      * 見つからなければ WS-LIM-IDX にゼロを返す。
      *----------------------------------------------------------------
       LOOKUP-LIMIT SECTION.
       LKL-START.
           MOVE ZERO TO WS-LIM-IDX
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-LIM-CNT
               IF WS-LIM-MEDIA (WS-I) = SESS-CARD-MEDIA
                  AND WS-LIM-AUTH (WS-I) = SESS-AUTH-METHOD
                   MOVE WS-I TO WS-LIM-IDX
                   EXIT PERFORM
               END-IF
           END-PERFORM.
       LKL-EXIT.
           EXIT.

      *----------------------------------------------------------------
      * 限度額マスタの読込。起動後 1 回だけ。判定は取引のたびに走る
      * ので都度 I/O させない。
      *----------------------------------------------------------------
       LOAD-LIMITS SECTION.
       LDL-START.
           IF WS-LIMIT-LOADED = 'Y'
               GO TO LDL-EXIT
           END-IF
           MOVE 'Y'  TO WS-LIMIT-LOADED
           MOVE ZERO TO WS-LIM-CNT

           OPEN INPUT LIMIT-FILE
           IF WS-LIMIT-STATUS NOT = '00' AND WS-LIMIT-STATUS NOT = '05'
               GO TO LDL-EXIT
           END-IF

           PERFORM UNTIL WS-LIMIT-STATUS NOT = '00'
               READ LIMIT-FILE
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       IF WS-LIM-CNT < WS-MAX-LIMITS
                           ADD 1 TO WS-LIM-CNT
                           MOVE LIMIT-MEDIA       TO WS-LIM-MEDIA (WS-LIM-CNT)
                           MOVE LIMIT-AUTH-METHOD TO WS-LIM-AUTH (WS-LIM-CNT)
                           MOVE LIMIT-PER-TXN     TO WS-LIM-PER-TXN (WS-LIM-CNT)
                           MOVE LIMIT-DAILY-AMT   TO WS-LIM-DAILY-AMT (WS-LIM-CNT)
                           MOVE LIMIT-DAILY-CNT   TO WS-LIM-DAILY-CNT (WS-LIM-CNT)
                       END-IF
               END-READ
           END-PERFORM
           CLOSE LIMIT-FILE.
       LDL-EXIT.
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
                          SESS-ACCT-NO SESS-HOLDER-NAME
                          SESS-CARD-MEDIA SESS-AUTH-METHOD.
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
      *
      * 仕様上の照合場所は媒体で異なる (磁気はホスト、IC は全銀協
      * 標準仕様によりカード内でも可) が、本実装は読取装置を持たず
      * どちらもハッシュ比較になるため経路を分けない。実 IC リーダを
      * 入れる際に差分が生まれた時点で分ける。先に空のラッパを 2 本
      * 置いても、閉塞カウンタの所在まで含めた境界は引けない。
      *
      * 不一致は照合場所によらず EC-PIN-INVALID を返す。利用者に
      * とっては同じ「暗証番号が違う」であり、認証技術の違いを画面に
      * 漏らす必要はない。
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
