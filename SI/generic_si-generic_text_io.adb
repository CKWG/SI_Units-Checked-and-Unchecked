------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2003, 2006, 2008, 2018, 2020, 2025, 2026
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

with Ada.Strings.Fixed;

package body Generic_SI.Generic_Text_IO is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   8.4
  -- Date      26 January 2026
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
  --  C.G.    8.0  13.08.2025 New implementation of unit evaluation:
  --                          Put to file reimplemented
  --  C.G.    8.1  19.08.2025 Implementation Valid_Modifier changed;
  --                          Put to and Get from string reimplemented;
  --                          raised Illegal_Unit instead of Data_Error
  --                          for Get with Width > 0
  --  C.G.    8.2  22.08.2025 Ensure_Task_Safety is new; Get for Width=0
  --                          new implementation
  --  C.G.    8.3  20.01.2026 Add µ in Get from file;
  --                          simplified code in Get from string
  --  C.G.    8.4  26.01.2026 Remove outdated and irritating comment
  --====================================================================

  use Real_Text_IO;

  function Strip (Dim: String) return String with Inline is
    use Ada.Strings, Ada.Strings.Fixed;
  begin
    return Trim (Dim, Both);
  end Strip;

  function Valid_Modifier (X: Item; Dim: String) return Boolean is
    sDim: constant String := Strip (Dim);
  begin
    if sDim = "" then
      return True;
    end if;
    declare  -- "rad" = 1, i.e. dimensionless
      S: constant String := (if sDim (sDim'First) in '*' | '/' then "rad" else "") & sDim;
    begin
      return has_Dimension (X, S);
    end;
  end Valid_Modifier;

  generic
    File: access constant File_Type;
  package Ensure_Task_Safety is  -- see Generic_SI
    procedure Get_and_Look_Ahead (C: out Character; EoF: out Boolean) with Inline;
  end Ensure_Task_Safety;

  package body Ensure_Task_Safety is
    -- A look ahead is neede to avoid consuming characters not belonging
    -- to the syntax. If a character belongs to the (correct or incorrect)
    -- syntax, it is used directly and consumed with the next call of
    -- Look_Ahead.
    First_Call: Boolean := True;
    procedure Get_and_Look_Ahead (C: out Character; EoF: out Boolean) is
    begin
      if First_Call then
        First_Call := False;
      else
        Get (File.all, C);
      end if;
      Look_Ahead (File.all, C, EoF);
      if EoF then
        First_Call := True;  -- reset for call with next unit string
      end if;
    end Get_and_Look_Ahead;
  end Ensure_Task_Safety;

  procedure Get (X: out Item; Width: in Field := 0) is
  begin
    Get (Current_Input, X, Width);
  end Get;

  procedure Get (File: in File_Type; X: out Item; Width: in Field := 0) is
  begin
    if Width = 0 then
      declare
        package Get_Interface is new Ensure_Task_Safety (File'Access);
        Num : Real'Base;
        Unit: Item;
        C   : Character;
        EoF : Boolean;
        Length: Natural := 0;  -- not needed
      begin
        Get (File, Num);
        Look_Ahead (File, C, EoF);
        if EoF or else C not in '*' | '/' | 'A' .. 'Z' | 'a' ..'z' | 'µ' then
          X := Num * One;
        else
          Construct (Get_Interface.Get_and_Look_Ahead'Access, Unit, Length);
          X := Num * Unit;
        end if;
      end;
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
        Get (Got (1 .. Avail), X, Last);
        if Last < Avail then
          raise Illegal_Unit with "string not exhausted";
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
    sDim: constant String := Strip (Dim);
  begin
    if sDim = "" then
      Put (File, X.Value, Fore, Aft, Exp);
      Put (File, Image (X.Unit));
    else
      declare
        eDim  : constant String := (if sDim (sDim'First) in '*' | '/' then "" else "*") & sDim;
        Gauge : Item;
        Length: Positive;
        Gauged: Dimensionless;
        package String_Interface is new Generic_SI.Ensure_Task_Safety (eDim);
      begin
        Construct (String_Interface.Get'Access, Gauge, Length);
        -- If we arrive here, syntax is OK, but there might be wrong characters left.
        if Length /= eDim'Length then
          raise Illegal_Unit with "string not exhausted";
        end if;
        Gauged := X / Gauge;
        Put (File, Gauged.Value, Fore, Aft, Exp);
        Put (File, eDim);
      end;
    end if;
  end Put;

  procedure Get (From: in  String;
                 X   : out Item;
                 Last: out Positive) is
    Value: Real'Base;
  begin
    Get (From, Value, Last);  -- get the numeric value
    if Last = From'Last or else From (Last + 1) not in '*' | '/' | 'A' .. 'Z' | 'a' ..'z' | 'µ' then
      X := Value * One;  -- no unit follows
    else
      declare
        package String_Interface is new Generic_SI.Ensure_Task_Safety (From (Last + 1 .. From'Last));
        Length: Positive;
      begin
        Construct (String_Interface.Get'Access, X, Length);
        X := Value * X;
        Last := Last + Length;
      end;
    end if;
  end Get;

  procedure Put (To : out String;
                 X  : in Item;
                 Aft: in Field  := Default_Aft;
                 Exp: in Field  := Default_Exp;
                 Dim: in String := "") is
    sDim: constant String := Strip (Dim);
  begin
    if sDim = "" then
      declare
        eDim : constant String := Image (X.Unit);
      begin
        Put (To (To'First .. To'Last - eDim'Length), X.Value, Aft, Exp);
        To  (To'Last - eDim'Length + 1 .. To'Last) := eDim;
      end;
    else
      declare
        eDim  : constant String := (if sDim (sDim'First) in '*' | '/' then "" else "*") & sDim;
        Gauge : Item;
        Length: Positive;
        Gauged: Dimensionless;
        package String_Interface is new Generic_SI.Ensure_Task_Safety (eDim);
      begin
        Construct (String_Interface.Get'Access, Gauge, Length);
        -- If we arrive here, syntax is OK, but there might be wrong characters left.
        if Length /= eDim'Length then
          raise Illegal_Unit with "string not exhausted";
        end if;
        Gauged := X / Gauge;
        Put (To (To'First .. To'Last - eDim'Length), Gauged.Value, Aft, Exp);
        To  (To'Last - eDim'Length + 1 .. To'Last) := eDim;
      end;
    end if;
  exception
    when Constraint_Error => raise Layout_Error;
  end Put;

end Generic_SI.Generic_Text_IO;
