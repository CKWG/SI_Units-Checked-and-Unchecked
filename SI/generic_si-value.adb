------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2018, 2019, 2020 Christoph Karl Walter Grein
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 3
-- of the License, or any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
--
-- As a special exception,  if other files  instantiate  generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License.  This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
--
-- Source:
-- https://www.adaic.org/ada-resources/tools-libraries/
--   (see Christoph Grein's Essentials)
-- http://archive.adaic.com/tools/CKWG/Dimension/Dimension.html
--
-- Author's email address:
--   christ-Usch.grein@t-online.de
------------------------------------------------------------------------------

separate (Generic_SI)
function Value (Dim: String) return Item is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.0
  -- Date      10 May 2020
  --====================================================================
  -- Scan the string for unit factors and evaluate each one.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  29.07.2018
  --  C.G.    1.1  29.09.2018 Unit syntax changed
  --  C.G.    1.2  13.02.2019 code improved and commented
  --  C.G.    2.0  10.05.2020 Made Checked_SI.Generic_Strings.Get_Item
  --                          child of Generic_SI and renamed Value
  --====================================================================

  procedure Get_Unit_Factor (X: String; Last: out Natural; Factor: out Item) is separate
    with Pre => X /= "";
    -- Last is the last evaluated character

  Last: Positive := Dim'First;

  function Evaluate_Unit (in_Numerator: Boolean) return Item is
    -- in_Numerator: True  - we are in numerator (a single unit or product);
    --                       this is the value of the first call;
    --               False - we are in divisor (a single unit or product in parentheses);
    --                       this is the value in the recursive call
    Product : Item := One;
    Factor  : Item;
    Operator: Character;
  begin
    loop
      Get_Unit_Factor (Dim (Last .. Dim'Last), Last, Factor);
      Product := Product * Factor;
      if Last = Dim'Last then  -- Dim consumed, legal unit
        return Product;
      end if;
      Operator := Dim (Last + 1);  -- * next factor follows; / divisor follows; ) divisor ends; else illegal
      Last := Last + 2;  -- consume operator
      if Last > Dim'Last and in_Numerator then  -- we are beyond end in numerator (in denom, ex. "m)", it's OK)
        raise Illegal_Unit;
      end if;
      case Operator is
        when '*' => null;                       -- next factor follows
        when '/' => if Dim (Last) = '(' then    -- now we are in divisor; ex. "m/(..." - product follows
                      if not in_Numerator then  -- ex. "m/(s/(..." - no more / allowed
                        raise Illegal_Unit;
                      end if;
                      Last := Last + 1;  -- consume (
                      Product := Product / Evaluate_Unit (in_Numerator => False);  -- recursive call
                      if Dim (Last - 1 .. Dim'Last) /= ")" then
                        raise Illegal_Unit;  -- nothing may follow
                      end if;
                      return Product;
                    else  -- a single unit in divisor
                      Get_Unit_Factor (Dim (Last .. Dim'Last), Last, Factor);
                      if Last /= Dim'Last then
                        raise Illegal_Unit;  -- nothing may follow
                      end if;
                      return Product / Factor;
                    end if;
        when ')' => if not in_Numerator then  -- we are in divisor (in recursive call)
                      return Product;
                    end if;
                    raise Illegal_Unit;  -- only allowed in divisor
        when others => raise Illegal_Unit;
      end case;
    end loop;
  end Evaluate_Unit;

begin

  if Dim = "" then
    return One;
  elsif Dim (Dim'Last) = ' ' then
    raise Illegal_Unit;
  end if;

  return Evaluate_Unit (in_Numerator => True);

end Value;

