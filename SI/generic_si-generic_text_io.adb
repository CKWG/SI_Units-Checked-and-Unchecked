------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2003, 2006, 2008, 2018, 2020, 2025
--               Christoph Karl Walter Grein
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
-- Author's email address:
--   christ-Usch.grein@t-online.de
------------------------------------------------------------------------------

with Ada.Strings.Fixed,
     Ada.Strings.Maps.Constants;
use  Ada.Strings.Maps,
     Ada.Strings.Maps.Constants;

with Rational_Arithmetics.Strings, Rational_Arithmetics.Text_IO;

package body Generic_SI.Generic_Text_IO is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   7.1
  -- Date      5 July 2025
  --====================================================================
  --
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    0.0  05.07.2002 Only Put implemented
  --  C.G.    1.0  23.07.2002 Done
  --  C.G.    1.1  29.07.2002 Also offending: unit^+1 unit^1/3
  --  C.G.    1.2  03.08.2002 Allow '/' when reading dimension
  --  C.G.    1.3  02.09.2002 Dim format modifier for Put added
  --  C.G.    1.4  15.11.2002 Use Real'Base
  --  C.G.    1.5  12.03.2003 IO customization
  --  C.G.    1.6  27.02.2006 Image now in Rational_Arithmetics.Strings
  --  C.G.    2.0  28.04.2008 Added Width to Get and io with strings
  --  C.G.    2.1  24.07.2008 Simplification
  --  C.G.    3.0  01.08.2008 Additional_Units
  --  C.G.    4.0  03.08.2018 Unit strings (Additional_Units removed)
  --  C.G.    5.0  13.05.2020 Parent renamed to Generic_SI
  --  C.G.    6.0  18.04.2025 Precondition replaces check in body
  --  C.G.    7.0  01.07.2025 generic param. as in Text_IO for Celsius
  --  C.G.    7.1  05.07.2025 Changed exception from Illegal_Unit to
  --                          Data_Error for Width > 0
  --====================================================================

  use Real_Text_IO;

  Operator_Set: constant Character_Set := To_Set ("*/");
  Unit_Set    : constant Character_Set := Alphanumeric_Set or Operator_Set or To_Set ("+-()");

  function Valid_Modifier (X: Item; Dim: String) return Boolean is
  begin
    if Dim = "" then
      return True;
    end if;
    declare  -- "rad" = 1, i.e. dimensionless
      S: constant String := (if Is_In (Dim (Dim'First), Operator_Set) then "rad" else "") & Dim;
    begin
      return Same_Dimension (X, 1.0 * S);
    end;
  end Valid_Modifier;

  procedure Get (X: out Item; Width: in Field := 0) is
  begin
    Get (Current_Input, X, Width);
  end Get;

  procedure Get_0 (File: in File_Type; X: out Item) is
    -- Get characters as long as they are inside the Unit_Set.
    Dim : String (1 .. 50) := (others => ' ');  -- should suffice for any imaginable input
    Last: Natural := 0;
    Char: Character;
    Stop: Boolean;
    Num : Real'Base;
    use Ada.Strings.Fixed;
  begin
    Get (File, Num);
    Look_Ahead (File, Char, Stop);
    if Stop or else not Is_In (Char, Operator_Set) then
      X := Num * One;
      return;
    end if;
    Unit: loop
      Look_Ahead (File, Char, Stop);
      if Stop or else not Is_In (Char, Unit_Set) then
        X := Num * ("rad" & Dim (1 .. Last));  -- rad = 1
        return;
      end if;
      Last := Last + 1;
      Get (File, Dim (Last));
    end loop Unit;
  end Get_0;

  procedure Get (File: in File_Type; X: out Item; Width: in Field := 0) is
  begin
    if Width = 0 then
      Get_0 (File, X);
    else
      declare
        Got  : String (1 .. Width);
        Avail: Natural := 0;
        Last : Natural;
      begin
        for C in 1 .. Width loop
          exit when End_Of_Line (File);
          Avail := Avail + 1;
          Get (File, Got (C));
        end loop;
        Get (Got (1 .. Avail), X, Last);  -- may raise Data_Error
        -- Gnat: 5_ does not raise Data_Error here.
        -- Discriminating between Data_Error and Illegal_Unit is difficult.
        -- In any case, this is a syntax error in the input.
        -- Choose the simple way.
        if Last < Avail then
          raise Data_Error;  -- seems better than Illegal_Unit
        end if;
      end;
    end if;
  end Get;

  procedure Put (X   : in Item;
                 Fore: in Field  := Default_Fore;
                 Aft : in Field  := Default_Aft;
                 Exp : in Field  := Default_Exp;
                 Dim : in String := "") is
  begin
    Put (Current_Output, X, Fore, Aft, Exp, Dim);
  end Put;

  procedure Put (File: in File_Type;
                 X   : in Item;
                 Fore: in Field  := Default_Fore;
                 Aft : in Field  := Default_Aft;
                 Exp : in Field  := Default_Exp;
                 Dim : in String := "") is
    Result: String (1 .. 255);  -- should suffice for any imaginable output
    Dot, Start: Positive;
  begin
    Put (Result, X, Aft, Exp, Dim);
    Start := Ada.Strings.Fixed.Index_Non_Blank (Result);       -- Start |    | Dot
    Dot   := Ada.Strings.Fixed.Index           (Result, ".");  --       10000.
    Start := Integer'Min (Start, Dot - Fore);
    Put (File, Result (Start .. Result'Last));
  end Put;

  procedure Get (From: in  String;
                 X   : out Item;
                 Last: out Positive) is
    Value: Real'Base;
    use Ada.Strings, Ada.Strings.Fixed;
  begin
    Get (From, Value, Last);  -- get the numeric value
    if Last = From'Last or else
      (Last + 1 <= From'Last and then not Is_In (From (Last + 1), Operator_Set)) then
      X := Value * One;  -- no unit follows
    else  -- a unit follows; get characters as long as they are inside the Unit_Set
      declare
        First: constant Positive := Last + 1;
        Stop : constant Natural := Index (Source => From, Set => Unit_Set, From => First, Test => Outside);
      begin
        if Stop = 0 then      -- no character outside the Unit_Set
          Last := From'Last;  -- all characters will be consumed
        else
          Last := Stop - 1;
        end if;
        X := Value * ("rad" & From (First .. Last));
      end;
    end if;
  end Get;

  procedure Put (To : out String;
                 X  : in Item;
                 Aft: in Field  := Default_Aft;
                 Exp: in Field  := Default_Exp;
                 Dim: in String := "") is
  begin
    if Dim = "" then
      declare
        Unit_Image : constant String  := Image (X.Unit);
      begin
        if Unit_Image = "" then  -- X has dimension 1
          Put (To, X.Value, Aft, Exp);
        else
          Put (To (To'First .. To'Last - Unit_Image'Length), X.Value, Aft, Exp);
          To  (To'Last - Unit_Image'Length + 1 .. To'Last) := Unit_Image;
        end if;
      end;
    elsif not Is_In (Dim (Dim'First), Operator_Set) then
      Put (To, X, Aft, Exp, '*' & Dim);
      return;
    else  -- e.g. "1.0*m/s", "1.0/s"
      declare
        Y: constant Item := X / (1.0 * ("rad" & Dim));  -- now dimension 1
      begin
        Put (To (To'First .. To'last - Dim'Length), Y.Value, Aft, Exp);
        To  (To'Last - Dim'Length + 1 .. To'Last) := Dim;
      end;
    end if;
  exception
    when Constraint_Error => raise Layout_Error;
  end Put;

end Generic_SI.Generic_Text_IO;
