------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2018, 2019, 2020, 2022, 2025 Christoph Karl Walter Grein
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

separate (Generic_SI.Generic_Symbols)
function Evaluate (Symbol: String) return Item is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   4.4
  -- Date      11 April 2025
  --====================================================================
  -- Symbol index starts always with 1.
  -- This is the time killer, so try to be efficient.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  30.07.2018
  --  C.G.    2.0  15.02.2019 Put into package Symbols
  --  C.G.    3.0  11.05.2020 Moved from Generic_Strings
  --  C.G.    4.0  24.07.2020 Optimimized for time
  --  C.G.    4.1  09.08.2020 eV added
  --  C.G.    4.2  06.12.2022 prefixes QqRr added
  --  C.G.    4.3  27.02.2025 deka -> deca
  --  C.G.    4.4  11.04.2025 ha added
  --====================================================================

  pragma Optimize (Time);

  Prefix: Real := 1.0;

  function Eval (Symbol: String; Prefixed: in Boolean) return Item is
    -- First call with Prefixed = False; if no unit found, the character must be a prefix,
    -- so second recursive call with Prefixed = True.
  begin
    if Symbol = "" then
      raise Illegal_Unit;
    end if;
    case Symbol (Symbol'First) is
      when 'A' => if Symbol = Symbol_List (A  ).Symbol then return Symbol_List (A  ).Value; end if;
      when 'B' => if Symbol = Symbol_List (Bq ).Symbol then return Symbol_List (Bq ).Value; end if;
      when 'C' => if Symbol = Symbol_List (C  ).Symbol then return Symbol_List (C  ).Value; end if;
      when 'E' => Prefix := exa;
      when 'F' => if Symbol = Symbol_List (F  ).Symbol then return Symbol_List (F  ).Value; end if;
      when 'G' => if Symbol = Symbol_List (Gy ).Symbol then return Symbol_List (Gy ).Value; end if;
                  Prefix := giga;
      when 'H' => if Symbol = Symbol_List (H  ).Symbol then return Symbol_List (H  ).Value;
               elsif Symbol = Symbol_List (Hz ).Symbol then return Symbol_List (Hz ).Value; end if;
      when 'J' => if Symbol = Symbol_List (J  ).Symbol then return Symbol_List (J  ).Value; end if;
      when 'K' => if Symbol = Symbol_List (K  ).Symbol then return Symbol_List (K  ).Value; end if;
      when 'L' => if Symbol = Symbol_List (L  ).Symbol then return Symbol_List (L  ).Value; end if;
      when 'M' => Prefix := mega;
      when 'N' => if Symbol = Symbol_List (N  ).Symbol then return Symbol_List (N  ).Value; end if;
      when 'O' => if Symbol = Symbol_List (Om ).Symbol then return Symbol_List (Om ).Value; end if;
      when 'P' => if Symbol = Symbol_List (Pa ).Symbol then return Symbol_List (Pa ).Value; end if;
                  Prefix := peta;
      when 'Q' => Prefix := quetta;
      when 'R' => Prefix := ronna;
      when 'S' => if Symbol = Symbol_List (Si ).Symbol then return Symbol_List (Si ).Value;
               elsif Symbol = Symbol_List (Sv ).Symbol then return Symbol_List (Sv ).Value; end if;
      when 'T' => if Symbol = Symbol_List (T  ).Symbol then return Symbol_List (T  ).Value; end if;
                  Prefix := tera;
      when 'V' => if Symbol = Symbol_List (V  ).Symbol then return Symbol_List (V  ).Value; end if;
      when 'W' => if Symbol = Symbol_List (W  ).Symbol then return Symbol_List (W  ).Value;
               elsif Symbol = Symbol_List (Wb ).Symbol then return Symbol_List (Wb ).Value; end if;
      when 'Y' => Prefix := yotta;
      when 'Z' => Prefix := zetta;
      when 'a' => Prefix := atto;
      when 'c' => if Symbol = Symbol_List (cd ).Symbol then return Symbol_List (cd ).Value; end if;
                  Prefix := centi;
      when 'd' => if Symbol'Length > 1 and then Symbol (Symbol'First + 1) = 'a' then Prefix := deca; else Prefix := deci; end if;
      when 'e' => if Symbol = Symbol_List (eV ).Symbol then return Symbol_List (eV ).Value; end if;
      when 'f' => Prefix := femto;
      when 'g' => if Symbol = Symbol_List (g  ).Symbol then return Symbol_List (g  ).Value; end if;
      when 'h' => if Prefixed then raise Illegal_Unit; end if;  -- no prefix on hour or hectare
                  if Symbol = Symbol_List (ha ).Symbol then return Symbol_List (ha ).Value;
               elsif Symbol = Symbol_List (hor).Symbol then return Symbol_List (hor).Value; end if;
                  Prefix := hecto;
      when 'k' => if Symbol = Symbol_List (kat).Symbol then return Symbol_List (kat).Value; end if;
                  Prefix := kilo;
      when 'l' => if Symbol = Symbol_List (lit).Symbol then return Symbol_List (lit).Value;
               elsif Symbol = Symbol_List (lm ).Symbol then return Symbol_List (lm ).Value;
               elsif Symbol = Symbol_List (lx ).Symbol then return Symbol_List (lx ).Value; end if;
      when 'm' => if Symbol = Symbol_List (m  ).Symbol then return Symbol_List (m  ).Value;
               elsif Symbol = Symbol_List (min).Symbol then if Prefixed then raise Illegal_Unit; end if;  -- no prefix on minute
                                                            return Symbol_List (min).Value;
               elsif Symbol = Symbol_List (mol).Symbol then return Symbol_List (mol).Value; end if;
                  Prefix := milli;
      when 'n' => Prefix := nano;
      when 'p' => Prefix := pico;
      when 'q' => Prefix := quecto;
      when 'r' => if Symbol = Symbol_List (rad).Symbol then return Symbol_List (rad).Value; end if;
                  Prefix := ronto;
      when 's' => if Symbol = Symbol_List (s  ).Symbol then return Symbol_List (s  ).Value;
               elsif Symbol = Symbol_List (sr ).Symbol then return Symbol_List (sr ).Value; end if;
      when 'u' => Prefix := micro;
      when 'y' => Prefix := yocto;
      when 'z' => Prefix := zepto;
      when others => null;
    end case;
    if Prefix /= 1.0 and not Prefixed then
      return Eval (Symbol ((if Prefix = deca then 3 else 2) .. Symbol'Last), Prefixed => True);
    end if;
    raise Illegal_Unit;
  end Eval;

begin

  return Prefix * Eval (Symbol, Prefixed => False);

end Evaluate;
