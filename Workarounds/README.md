# KastriFree Workarounds

The files in this folder contain instructions for *potential* workarounds for issues with Delphi.

Typically the files will refer to the report number in the Quality Portal:

  https://quality.embarcadero.com

e.g. RSP-19525.txt refers to:

  https://quality.embarcadero.com/browse/RSP-19525

Each file will have a link to the report in it, plus instructions on how to apply the workaround.

For workarounds that have a .patch file, please use the SourcePatcher utility from here:

  https://github.com/DelphiWorlds/Tools/blob/master/SourcePatcher.zip

(Click the Download button on the right to download)

Run SourcePatcher, and if you do not already have the Patch executable (a separate program), click the Download Patch button to download it, then install.
Once Patch has been installed, go to:

  C:\Program Files (x86)\GnuWin32\bin

and rename patch.exe to p.exe (or similar). This is because later versions of Windows do not like executables called patch.exe

When you switch back to SourcePatcher, it should auto-detect the installation of Patch. If not, use the ellipsis button for the Patch Executable edit to select the Patch executable.

Next, select the original source file that is to be patched. SourcePatcher will detect the *latest* version of Delphi you have installed, and start in the source folder for that installation.

Select the desired *.patch file (e.g. from the Workarounds folder in Kastri Free), select the Destination folder where the patched source file should be output to, and click Patch Source


***USE THESE WORKAROUNDS AT YOUR OWN RISK - NO LIABILITY WILL BE ACCEPTED***

However, please report any issues to:

  https://github.com/DelphiWorlds/KastriFree/issues


