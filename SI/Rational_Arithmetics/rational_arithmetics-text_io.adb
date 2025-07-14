------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2018 Christoph Karl Walter Grein
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
-- Author's homepage and email address:
--   http://home.T-Online.de/home/Christ-Usch.Grein/
--   Christ-Usch.Grein@T-Online.de
------------------------------------------------------------------------------

with Ada.Integer_Text_IO;
use  Ada.Integer_Text_IO;

package body Rational_Arithmetics.Text_IO is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.1
  -- Date      30 June 2002
  --====================================================================
  --
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    0.0  27.06.2002 Only Put operations
  --  C.G.    1.0  30.06.2002 Done
  --  C.G.    1.1  18.07.2002 Interprete number read as integer when
  --                          following character is not '/'
  --====================================================================

  procedure Get (Item: out Rational) is
  begin
    Get (Current_Input, Item);
  end Get;

  procedure Put (Item: in Rational) is
  begin
    Put (Current_Output, Item);
  end Put;

  procedure Get (File: in File_Type; Item: out Rational) is
    N, D: Whole;
    C   : Character;
    EoL : Boolean;
  begin
    Get (File, Integer (N));
    Look_Ahead (File, C, EoL);
    if EoL or else C /= '/' then
      Item := +N;
      return;
    end if;
    Get (File, C);  -- consume '/'
    Look_Ahead (File, C, EoL);
    if EoL or else C not in '0' .. '9' then
      raise Data_Error;
    end if;
    Get (File, Integer (D));
    Item := N/D;
  end Get;

  procedure Put (File: in File_Type; Item: in Rational) is
  begin
    if Item.Denominator = 1 then
      Put (File, Whole'Image (Item.Numerator));
    else
      declare
        D: constant String := Whole'Image (Item.Denominator);
      begin
        Put (File, Whole'Image (Item.Numerator) & '/' & D (2..D'Last));
      end;
    end if;
  end Put;

end Rational_Arithmetics.Text_IO;
