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
with LSP_Gen.Type_Aliases;

package LSP_Gen.Structures is

   type Or_Mapping_Kind is
     (Type_Or_Null,
      Type_Class,
      Two_Types,
      Option_Combination,
      Unknown_Mapping);

   type Or_Mapping (Kind : Or_Mapping_Kind := Or_Mapping_Kind'First) is record
      case Kind is
         when Type_Or_Null | Type_Class | Option_Combination =>
            Tipe : LSP_Gen.Entities.AType;
         when Two_Types =>
            First, Second : LSP_Gen.Entities.AType;
         when Unknown_Mapping =>
            null;
      end case;
   end record;

   type Type_Info is record
      Name         : VSS.Strings.Virtual_String;
      Definition   : LSP_Gen.Entities.Structure;
      Is_Tagged    : Boolean := False;  --  used in `extends`
      Is_Mixin     : Boolean := False;
      Has_Optional : Boolean := False;
      Has_Array    : Boolean := False;
   end record;

   package Type_Maps is new Ada.Containers.Ordered_Maps
     (VSS.Strings.Virtual_String,
      Type_Info,
      VSS.Strings."<");

   Types : Type_Maps.Map;

   Aliases : LSP_Gen.Type_Aliases.Alias_Maps.Map renames
     LSP_Gen.Type_Aliases.Aliases;

   procedure Write_Types (Model : LSP_Gen.Entities.MetaModel);
   --  Write type declarations for each type

end LSP_Gen.Structures;
