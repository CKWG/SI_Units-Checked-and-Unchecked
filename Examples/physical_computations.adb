------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2018, 2020 Christoph Karl Walter Grein
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
--   http://archive.adaic.com/tools/CKWG/CKWG.html
--   Christ-Usch.Grein@T-Online.de
------------------------------------------------------------------------------

with Ada.Text_IO;
use  Ada.Text_IO;

with SI.IO, SI.Nat;
use  SI.IO, SI.Nat, SI;

procedure Physical_Computations is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.3
  -- Date      9 August 2020
  --====================================================================
  -- An example use.
  --
  -- 1. Airspeed is measured by a Pitot tube. Since the pressure in the
  --    tube depends on the air density (p = rho*v**2/2) and is gauged
  --    to the norm atmosphere, the indicated air speed has to be
  --    corrected by
  --       SQRT (Gauge_Air_Density/Surrounding_Air_Density)
  --    to give the true airspedd (TAS).
  -- 2. Speed of sound in norm atmosphere
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  25.07.2002 with Gnat 3.14p bug work-around
  --                          (unnecesary qualification needed)
  --  C.G.    1.1  27.09.2002 Gnat work-around removed for 3.16w
  --  C.G.    2.0  06.08.2018 Unit strings
  --  C.G.    2.1  01.10.2018 Unit syntax changed N*m/K/kg => N*m/(K*kg)
  --  C.G.    2.2  07.03.2020 use Gas_Constant from Nat_Const package
  --  C.G.    2.3  09.08.2020 Electron and Proton mass in eV
  --====================================================================

  Electron_Mass_eV: constant Energy := Electron_Mass * Speed_of_Light**2;
  Proton_Mass_eV  : constant Energy := Proton_Mass   * Speed_of_Light**2;

  F: constant := 5;  -- degrees of freedom for bar-like (o-o) air molecules
  Adiabatic_Exp: constant Float := Float (F + 2) /  Float (F);

  T_Norm  : constant Temperature  :=  288.15   * "K";    -- 15 deg Celsius
  P_Norm  : constant Pressure     := 1013.25   * "hPa";  -- 760 Torr = 1013 hPa
  Rho_Norm: constant Mass_Density :=    1.2255 * "kg/m**3";

  Molar_Mass   : constant Item := 28.9644 * "g/mol";  -- dry air
  Gas_Const_Air: constant Item := Gas_Constant / Molar_Mass;  -- 287.058 J/(K*kg)

  Air_Pressure: constant Pressure     := 0.9 * P_Norm;
  OAT         : constant Temperature  := 273.15 * "K";  -- outside air temperature
  Rho         : constant Mass_Density := Air_Pressure / (Gas_Const_Air * OAT);

  -- air density ratio (Rho/Rho_Norm)
  Sigma, Sqrt_Sigma: Dimensionless;

  IAS: constant Speed := 200.0 * "km/h";  -- indicated and
  TAS: Speed;                             -- true air speed

  Sound: Speed;

begin

  Default_Fore := 4;
  Default_Aft  := 0;
  Default_Exp  := 0;

  Sigma      := Rho / Rho_Norm;
  Sqrt_Sigma := Sqrt (Sigma);

  TAS := IAS / Sqrt_Sigma;

  Sound := Sqrt (Adiabatic_Exp * P_Norm / Rho_Norm);

  Put ("Electron and Proton mass");  Put (Electron_Mass_eV, Dim => "MeV");  Put (Proton_Mass_eV, Dim => "MeV");  New_Line (2);

  Put ("Gas_Constant");  Put (Gas_Constant);  New_Line (2);

  Put ("Norm tempearture and OAT    ");  Put (T_Norm);                Put (OAT);                         New_Line;
  Put ("Norm  and air pressure      ");  Put (P_Norm, Dim => "hPa");  Put (Air_Pressure, Dim => "hPa");  New_Line;
  Put ("Mass density air            ");  Put (Rho_Norm);              Put (Rho, Dim => "kg/m**3");       New_Line;
  Put ("Sigma                       ");  Put (Sigma);                 Put (Sqrt_Sigma, Dim => "");       New_Line;
  Put ("Indicated and true air speed");  Put (IAS, Dim => "km/h");    Put (TAS, Dim => "km/h");          New_Line (2);

  Put ("Speed of sound              ");  Put (Sound);  New_Line;

end Physical_Computations;
