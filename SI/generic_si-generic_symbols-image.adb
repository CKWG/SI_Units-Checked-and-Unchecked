------------------------------------------------------------------------------
-- Checked and Unchecked ComImageation with SI Units
-- Copyright (C) 2018, 2020 Christoph Karl Walter Grein
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

with Ada.Strings.Fixed;

with Rational_Arithmetics.Strings;

separate (Generic_SI.Generic_Symbols)
function Image (X: Dimensions.Dimension) return String is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.0
  -- Date      13 May 2020
  --====================================================================
  --
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  29.07.2018 Extracted from Text_IO
  --  C.G.    2.0  13.05.2020 Parent renamed to Generic_Symbols
  --====================================================================

  function Image (Symbol: String; Value: in Rational) return String is
  begin
    if Value = 0 then
      return "";
    else
      declare
        use Ada.Strings, Ada.Strings.Fixed;
        Exp  : constant String := (if Value = 1 then "" else "**");
        Open : constant String := (if Value < 0 or Denominator (Value) /= 1 then "(" else "");
        Close: constant String := (if Value < 0 or Denominator (Value) /= 1 then ")" else "");
        Image: constant String := (if Value = 1 then "" else Trim (Rational_Arithmetics.Strings.Image (Value), Left));
      begin
        return
          '*' & Symbol & Exp & Open & Image & Close;
      end;
    end if;
  end Image;

begin

  return Image ("m"  , Dimensions.m   (X)) &
         Image ("kg" , Dimensions.kg  (X)) &
         Image ("s"  , Dimensions.s   (X)) &
         Image ("A"  , Dimensions.A   (X)) &
         Image ("K"  , Dimensions.K   (X)) &
         Image ("cd" , Dimensions.cd  (X)) &
         Image ("mol", Dimensions.mol (X));

end Image;
