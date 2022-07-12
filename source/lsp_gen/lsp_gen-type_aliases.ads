------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Ordered_Maps;

with VSS.Strings;

with LSP_Gen.Entities;

package LSP_Gen.Type_Aliases is

   type Alias_Info is record
      Name         : VSS.Strings.Virtual_String;
      Definition   : LSP_Gen.Entities.TypeAlias;
      Is_Mixin     : Boolean := False;
      Has_Optional : Boolean := False;
      Has_Array    : Boolean := False;
   end record;

   package Alias_Maps is new Ada.Containers.Ordered_Maps
     (VSS.Strings.Virtual_String,
      Alias_Info,
      VSS.Strings."<");

   Aliases : Alias_Maps.Map;

end LSP_Gen.Type_Aliases;
