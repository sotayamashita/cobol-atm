      *****************************************************************
      * CASHIF.cpy - ATMCASH 呼出インタフェース
      *   CALL 'ATMCASH' USING CASH-PARM ATM-SESSION
      *   PLAN で得た CASH-OUT-PLAN をそのまま DISPENSE へ引き継ぐこと。
      *   呼出元が PLAN 結果を書き換えてはならない。
      *   ACCEPT は CASH-IN-DEPOSIT (金種別計数機が数えた内訳) を要求する。
      *****************************************************************
       01  CASH-PARM.
           05  CASH-FUNCTION           PIC X(08).
               88  CASH-FN-OPEN                VALUE 'OPEN    '.
               88  CASH-FN-PLAN                VALUE 'PLAN    '.
               88  CASH-FN-DISPENSE            VALUE 'DISPENSE'.
               88  CASH-FN-ACCEPT              VALUE 'ACCEPT  '.
               88  CASH-FN-CLOSE               VALUE 'CLOSE   '.
      *        -- 締めバッチ用。
      *        -- THEORY : 帳簿上あるべき枚数を返す (在庫は触らない)
      *        -- SETTLE : 当日計をクリアして営業日を繰り越す
               88  CASH-FN-THEORY              VALUE 'THEORY  '.
               88  CASH-FN-SETTLE              VALUE 'SETTLE  '.
           05  CASH-IN-AMOUNT          PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  CASH-OUT-RETCODE        PIC S9(04) COMP.
           05  CASH-OUT-ERROR-CODE     PIC X(04).
           05  CASH-OUT-PLAN.
               10  CASH-PL-DENOM OCCURS 4 TIMES PIC 9(06).
               10  CASH-PL-CNT   OCCURS 4 TIMES PIC 9(03).
      *    -- OPEN が返す端末構成。カセットの金種の並びは据付構成で
      *    -- 取引ごとに変わらないので、呼出元は起動時に一度受け取って
      *    -- 保持すればよい。在庫を知るための THEORY とは用途が違う。
           05  CASH-OUT-LAYOUT.
               10  CASH-LO-DENOM OCCURS 4 TIMES PIC 9(06).
      *    -- ACCEPT へ渡す入金内訳。金種別計数機が数えた結果であり、
      *    -- 受入不可で返却した紙幣はここに含まれない。返却枚数を別に
      *    -- 持たないのは、収納したものだけを数えれば在庫は合うため。
      *    -- 添字はカセット番号と一致させること (CASH-OUT-PLAN と同じ
      *    -- 規約)。金種で引き当てる表にすると、同じ並びを二通りに
      *    -- 表現することになる。CASH-LO-DENOM をそのまま写せばよい。
      *    -- 合計が CASH-IN-AMOUNT と一致しない内訳は受け付けない。
      *    -- 記帳額と収納額がずれると在庫と元帳が食い違うためである。
           05  CASH-IN-DEPOSIT.
               10  CASH-DP-DENOM OCCURS 4 TIMES PIC 9(06).
               10  CASH-DP-CNT   OCCURS 4 TIMES PIC 9(03).
      *    -- THEORY が返す、帳簿上あるべき枚数と当日の増減。
      *    -- 実査枚数との比較は呼出元 (締めバッチ) が行う。ここは
      *    -- 「帳簿がどうなっているか」だけを答える。
           05  CASH-OUT-THEORY.
               10  CASH-TH-DENOM OCCURS 4 TIMES PIC 9(06).
               10  CASH-TH-CNT   OCCURS 4 TIMES PIC 9(05).
           05  CASH-OUT-DISPENSED      PIC S9(13)V99 SIGN LEADING SEPARATE.
           05  CASH-OUT-DEPOSITED      PIC S9(13)V99 SIGN LEADING SEPARATE.
