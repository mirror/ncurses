------------------------------------------------------------------------------
-- Copyright (c) 2010-2011,2014 Free Software Foundation, Inc.              --
--                                                                          --
-- Permission is hereby granted, free of charge, to any person obtaining a  --
-- copy of this software and associated documentation files (the            --
-- "Software"), to deal in the Software without restriction, including      --
-- without limitation the rights to use, copy, modify, merge, publish,      --
-- distribute, distribute with modifications, sublicense, and/or sell       --
-- copies of the Software, and to permit persons to whom the Software is    --
-- furnished to do so, subject to the following conditions:                 --
--                                                                          --
-- The above copyright notice and this permission notice shall be included  --
-- in all copies or substantial portions of the Software.                   --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  --
-- OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               --
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   --
-- IN NO EVENT SHALL THE ABOVE COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,   --
-- DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR    --
-- OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR    --
-- THE USE OR OTHER DEALINGS IN THE SOFTWARE.                               --
--                                                                          --
-- Except as contained in this notice, the name(s) of the above copyright   --
-- holders shall not be used in advertising or otherwise to promote the     --
-- sale, use or other dealings in this Software without prior written       --
-- authorization.                                                           --
------------------------------------------------------------------------------
-- $Id: library.gpr.sed,v 1.2 2014/05/24 21:28:29 tom Exp $
-- http://gcc.gnu.org/onlinedocs/gnat_ugn_unw/Library-Projects.html
-- http://www.adaworld.com/debian/debian-ada-policy.html
project Library is
  Build_Dir := External ("BUILD_DIR");
  for Library_Name use External ("LIB_NAME");
  for Library_Version use External ("SONAME");
  for Library_Kind use "dynamic";
  for Library_Dir use Build_Dir & "/lib";
  for Object_Dir use Build_Dir & "/dynamic-obj";
  for Library_ALI_Dir use Build_Dir & "/dynamic-ali";
  for Source_Dirs use (".");
  for Library_Options use
    External_As_List ("LDFLAGS", " ")   --  before libraries.
    & ("-lncurses", "-lpanel", "-lmenu", "-lform");
  package Compiler is
     for Default_Switches ("Ada") use
       ("-g",
        "-O2",
        "-gnatafno",
        "-gnatVa",   -- All validity checks
        "-gnatwa")   -- Activate all optional errors
       & External_As_List ("ADAFLAGS", " "); --  after default options
  end Compiler;

  --  gnatmake ignores C sources, but this option will let it embed
  --  objects found in the Object_Dir.
  for Languages use ("C", "Ada");
end Library;
