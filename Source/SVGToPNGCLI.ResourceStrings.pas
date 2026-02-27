{******************************************************************************}
{                                                                              }
{   SVG To PNG Converter Command Line                                          }
{                                                                              }
{   Copyright (c) 2026 (Ethea S.r.l.)                                          }
{   Author: Carlo Barazzetta                                                   }
{                                                                              }
{   https://github.com/EtheaDev/SVGIconImageList                               }
{                                                                              }
{   Based on: Delphi Day 2020: Una CLI che i tuoi utenti ameranno              }
{             by Marco Breveglieri                                             }
{   https://www.breveglieri.it/eventi/2020-06-11-delphiday-creare-client-cli/  }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}
unit SVGToPNGCLI.ResourceStrings;

interface

resourcestring

  // Errors
  StrErrFileNotFound = 'Error: File "%s" not found!';
  StrErrNoFilesFound = 'Error: No files found matching pattern "%s"!';
  StrErrParseCommand = 'Error in params of command "%s":%s%s';
  StrErrCommandUnknown = 'Unknown or unavailable Command: "%s"';
  StrErrInvalidSize = 'Error: Invalid size value "%s"!';

  //Messages
  StrCommand = 'Command';
  StrStartCommand = 'Starting Command';
  StrDoneCommand = 'Command Done';
  StrOptional = '(Optional)';
  StrRequired = '(Required)';
  StrDefault = '(default)';

  // Command: help
  StrCmdHelpDescription = 'Show Help of a command';
  StrCmdHelpInfo = 'View informations about how the commands work '+
    'and the options you can specify.';
  StrCmdHelpCommandInfo = 'Command for which to call up the guide';

  // Command: convert
  StrCmdConvertDescription = 'Convert SVG files to PNG';
  StrCmdConvertInfo = 'Convert one or more SVG files into PNG images at one or more '+
    'specified sizes. Supports wildcard patterns for batch processing.';

  // Command: convert / options
  StrOptInputPattern = 'SVG input file or wildcard pattern (e.g. *.svg)';
  StrOptOutputFolder = 'Output folder for generated PNG files';
  StrOptSizes = 'PNG sizes separated by comma (e.g. 16,32,48,64)';
  StrOptFixedColor = 'Fixed color to apply (e.g. clBlack, #FF0000)';
  StrOptGrayScale = 'Convert to grayscale';
  StrOptOpacity = 'Opacity value from 0.0 to 1.0';

  // Processing messages
  StrProcessingFile = 'Processing file';
  StrExportedPng = 'Exported PNG';
  StrFilesFound = 'Files found';
  StrOutputFolder = 'Output folder';
  StrSizes = 'Sizes';

implementation

end.
