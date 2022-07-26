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
with LSP_Gen.Enumerations;
with LSP_Gen.Type_Aliases;

package LSP_Gen.Structures is

   type Or_Mapping_Kind is
     (Type_Union,
      Type_Or_Null,
      Array_Or_Null,
      Type_Class,
      Type_Or_Array,
      Two_Types,
      Two_Literals,
      String_Or_Array,
      String_Or_Tuple,
      String_Or_Something,
      Boolean_Or_Something,
      Boolean_Or_Class,
      Enumeration,  --  set of stringLiteral
      Option_Combination,
      Boolean_Or_Any,
      Location_Or_Something,
      Unknown_Mapping);

   type Or_Mapping (Kind : Or_Mapping_Kind := Or_Mapping_Kind'First) is record
      case Kind is
         when Type_Or_Null
            | Type_Class
            | Boolean_Or_Class
            | Option_Combination
            | Location_Or_Something
            | String_Or_Tuple
            | String_Or_Something
            | Boolean_Or_Something =>

            Tipe : LSP_Gen.Entities.AType;
         when Two_Types =>
            First, Second : LSP_Gen.Entities.AType;
         when Type_Or_Array
            | Array_Or_Null
            | String_Or_Array =>

            Array_Type    : LSP_Gen.Entities.AType;
         when Enumeration | Type_Union =>
            Items : LSP_Gen.Entities.AType_Vector;
         when Two_Literals
            | Boolean_Or_Any
            | Unknown_Mapping =>

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

   Enums : LSP_Gen.Enumerations.Enum_Maps.Map renames
     LSP_Gen.Enumerations.Enums;

   procedure Write_Types (Model : LSP_Gen.Entities.MetaModel);
   --  Write type declarations for each type

end LSP_Gen.Structures;
