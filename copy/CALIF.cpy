      *****************************************************************
      * CALIF.cpy - ATMCAL 呼出インタフェース
      *   CALL 'ATMCAL' USING CAL-PARM ATM-SESSION
      *
      *   営業日カレンダー。曜日と祝日マスタから「区分」を決める。
      *   実務規則: 平日・土曜が祝日にあたる場合は日曜・休日扱い。
      *   手数料も全銀システムの接続時間も、この区分を入力にとる。
      *****************************************************************
       01  CAL-PARM.
           05  CAL-FUNCTION            PIC X(08).
               88  CAL-FN-DAY-TYPE             VALUE 'DAYTYPE '.
      *        -- 指定日の翌営業日 (土日祝を飛ばす)
               88  CAL-FN-NEXT-BUSINESS        VALUE 'NEXTBIZ '.
           05  CAL-IN-DATE             PIC 9(08).
           05  CAL-OUT-RETCODE         PIC S9(04) COMP.
           05  CAL-OUT-DAY-TYPE        PIC X(01).
               88  CAL-DT-WEEKDAY              VALUE 'W'.
               88  CAL-DT-SATURDAY             VALUE 'S'.
               88  CAL-DT-HOLIDAY              VALUE 'H'.
      *    -- 1 = 月曜 … 7 = 日曜 (ISO と同じ並び)
           05  CAL-OUT-DOW             PIC 9(01).
      *    -- 祝日マスタに載っていたか (日曜と区別したい場合に使う)
           05  CAL-OUT-IS-HOLIDAY      PIC X(01).
           05  CAL-OUT-NEXT-DATE       PIC 9(08).
