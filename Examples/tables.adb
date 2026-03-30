------------------------------------------------------------------------------
-- Checked and Generic Computation with SI Units
-- Copyright (C) 2026 Christoph Karl Walter Grein
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
--   christ-usch.grein@t-online.de
------------------------------------------------------------------------------

with Ada.Text_IO;
use  Ada.Text_IO;

with SI.Nat, SI.IO;
use  SI.Nat, SI.IO, SI;

procedure Tables is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.0
  -- Date      30 March 2026
  --====================================================================
  -- Two examples for tables.
  -- Result in Tables.txt for digits 18.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  30.03.2026
  --====================================================================

  package Earth is
    Earth_Mass            : constant Mass         :=     5.9722E+24*"kg";  -- https://en.wikipedia.org/wiki/Earth_mass
    Equatorial_Radius     : constant Length       :=  6378.1370    *"km";  -- https://en.wikipedia.org/wiki/Earth_radius
    Polar_Radius          : constant Length       :=  6356.7523    *"km";  -- according to WGS 84
    Arithmetic_Mean_Radius: constant Length       := (2.0*Equatorial_Radius + Polar_Radius)/3.0;  -- 6371.0088 km
    Earth_Acceleration    : constant Acceleration := Gravity * Earth_Mass / Arithmetic_Mean_Radius**2;
  end Earth;

  use Earth;

  package Simple_Table is
    -- Column 1 .. Start-1 => Name
    -- Column Start        => X in given format
    Procedure Set (Start: in  Positive_Count);
    procedure Put (Name : in  String; X: in  Item; Fore, Aft, Exp: in Field; Dim: in String := "");
    procedure Get (Name : out String; X: out Item);
  end Simple_Table;

  package body Simple_Table is
    S: Positive_Count;
    procedure Set (Start: in Positive_Count) is
    begin
      S := Start;
    end Set;
    procedure Put (Name: in String; X: in Item; Fore, Aft, Exp: in Field; Dim: in String := "") is
    begin
      Put (Name);
      Set_Col (S);
      Put (X, Fore, Aft, Exp, Dim => Dim);
      New_Line;
    end Put;
    procedure Get (Name: out String; X: out Item) is
    begin
      Get (Name (Name'First .. Name'First + Positive (S) - 1));
      Get (X);
      Skip_Line;
    end Get;
  end Simple_Table;

  package Aligned_Table is
    -- Column 1 .. Start-1 => Name
    -- Column Start        => Width characers for X in given numeric format,
    --                        unit following aligned
    procedure Set (Start: in  Positive_Count; Width: in Field);
    procedure Put (Name : in  String; X: in  Item; Aft, Exp: in Field; Dim: in String := "");
    procedure Get (Name : out String; X: out Item);
  end Aligned_Table;

  package body Aligned_Table is
    S: Positive_Count;
    W: Field;
    procedure Set (Start: in Positive_Count; Width: in Field) is
    begin
      S := Start;
      W := Width;
    end Set;
    procedure Put (Name: in String; X: in Item; Aft, Exp: in Field; Dim: in String := "") is
      E: constant Field := (if Exp > 0 then Field'Max (3, Exp) else 0);
      F: constant Field := W - Aft - E - 1 - (if Exp = 0 then 0 else 1);  -- -1 for sign and exponent character E
    begin
      Put (Name);
      Set_Col (S);
      Put (X, Fore => F, Aft => Aft, Exp => E, Dim => Dim);
      New_Line;
    end Put;
    procedure Get (Name: out String; X: out Item) is
    begin
      Get (Name (Name'First .. Name'First + Positive (S) - 1));
      Get (X);
      Skip_Line;
    end Get;
  end Aligned_Table;

begin

  Put_Line ("Earth Data");
  Simple_Table.Set (Start => 24);
  Put_Line ("123456789012345678901234");
  Simple_Table.Put ("Mass"                  , Earth_Mass            , Fore => 0, Aft => 4, Exp => 1);
  Simple_Table.Put (""                      , Earth_Mass            , Fore => 0, Aft => 4, Exp => 1, Dim => "Rg"    );
  Simple_Table.Put ("Equatorial Radius"     , Equatorial_Radius     , Fore => 0, Aft => 4, Exp => 0, Dim => "km"    );
  Simple_Table.Put ("Polar Radius"          , Polar_Radius          , Fore => 0, Aft => 4, Exp => 0, Dim => "km"    );
  Simple_Table.Put ("Mean Radius"           , Arithmetic_Mean_Radius, Fore => 0, Aft => 4, Exp => 0, Dim => "km"    );
  Simple_Table.Put ("Gravitational Constant", Gravity               , Fore => 0, Aft => 5, Exp => 2);
  Simple_Table.Put ("Acceleration"          , Earth_Acceleration    , Fore => 0, Aft => 4, Exp => 0, Dim => "m/s**2");

  New_Line;

  Aligned_Table.Set (Start => 24, Width => 12);
  Put_Line ("12345678901234567890123|23456789012*");
  Aligned_Table.Put ("Mass"                  , Earth_Mass            , Aft => 4, Exp => 1);
  Aligned_Table.Put (""                      , Earth_Mass            , Aft => 4, Exp => 0, Dim => "Rg");
  Aligned_Table.Put ("Equatorial Radius"     , Equatorial_Radius     , Aft => 4, Exp => 0, Dim => "km");
  Aligned_Table.Put ("Polar Radius"          , Polar_Radius          , Aft => 4, Exp => 0, Dim => "km");
  Aligned_Table.Put ("Mean Radius"           , Arithmetic_Mean_Radius, Aft => 4, Exp => 0, Dim => "km");
  Aligned_Table.Put ("Gravitational Constant", Gravity               , Aft => 5, Exp => 2);
  Aligned_Table.Put ("Acceleration"          , Earth_Acceleration    , Aft => 4, Exp => 0, Dim => "m/s**2");

  New_Line;

  declare
    Result: File_Type;
    Earth_Mass            : Item;--Mass;
    Equatorial_Radius     : Length;
    Polar_Radius          : Length;
    Arithmetic_Mean_Radius: Length;
    Gravity               : Item;
    Earth_Acceleration    : Acceleration;
    Name: String (101 .. 123);
  begin
    Open (Result, In_File, "Tables.txt");
    Set_Input (Result);
    Skip_Line (2);
    --
    Get (Name);
    Get (Earth_Mass);
    Put_Line (Name & Boolean'Image(Earth_Mass = Earth.Earth_Mass));
    Skip_Line;
    --
    Get (Name);
    Get (Earth_Mass);
    Put_Line (Name & Boolean'Image(Earth_Mass = Earth.Earth_Mass));
    Skip_Line;
    --
    Get (Name);
    Get (Equatorial_Radius);
    Put_Line (Name & Boolean'Image(Equatorial_Radius = Earth.Equatorial_Radius));
    Skip_Line;
    --
    Get (Name);
    Get (Polar_Radius);
    Put_Line (Name & Boolean'Image(Polar_Radius = Earth.Polar_Radius));
    Skip_Line;
    --
    Get (Name);
    Get (Arithmetic_Mean_Radius);
    Put (Name & Boolean'Image(Arithmetic_Mean_Radius = Earth.Arithmetic_Mean_Radius));
    Put (Arithmetic_Mean_Radius - Earth.Arithmetic_Mean_Radius);
    New_Line;
    Skip_Line;
    --
    Get (Name);
    Get (Gravity);
    Put_Line (Name & Boolean'Image(Gravity = SI.Nat.Gravity));
    Skip_Line;
    --
    Get (Name);
    Get (Earth_Acceleration);
    Put (Name & Boolean'Image(Earth_Acceleration = Earth.Earth_Acceleration));
    Put (Earth_Acceleration - Earth.Earth_Acceleration);
    New_Line;
    Skip_Line;
    --
    Skip_Line (2);
    --
    Get (Name);
    Get (Earth_Mass);
    Put_Line (Name & Boolean'Image(Earth_Mass = Earth.Earth_Mass));
    Skip_Line;
    --
    Get (Name);
    Get (Earth_Mass);
    Put_Line (Name & Boolean'Image(Earth_Mass = Earth.Earth_Mass));
    Skip_Line;
    --
    Get (Name);
    Get (Equatorial_Radius);
    Put_Line (Name & Boolean'Image(Equatorial_Radius = Earth.Equatorial_Radius));
    Skip_Line;
    --
    Get (Name);
    Get (Polar_Radius);
    Put_Line (Name & Boolean'Image(Polar_Radius = Earth.Polar_Radius));
    Skip_Line;
    --
    Get (Name);
    Get (Arithmetic_Mean_Radius);
    Put (Name & Boolean'Image(Arithmetic_Mean_Radius = Earth.Arithmetic_Mean_Radius));
    Put (Arithmetic_Mean_Radius - Earth.Arithmetic_Mean_Radius);
    New_Line;
    Skip_Line;
    --
    Get (Name);
    Get (Gravity);
    Put_Line (Name & Boolean'Image(Gravity = SI.Nat.Gravity));
    Skip_Line;
    --
    Get (Name);
    Get (Earth_Acceleration);
    Put (Name & Boolean'Image(Earth_Acceleration = Earth.Earth_Acceleration));
    Put (Earth_Acceleration - Earth.Earth_Acceleration);
    New_Line;
    Skip_Line;
    --
    Close (Result);
    end;

end Tables;
