{-# LANGUAGE OverloadedStrings, StrictData #-}
-- |
-- Module: NetSpider.CLI
-- Description: CLI option parsers for NetSpider objects
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This module defines CLI option parsers for NetSpider objects, such
-- as configuration for Spider object and a query for a snapshot
-- graph. The option parsers are based on 'Parser' from
-- "Options.Applicative" (optparse-applicative package), so you can
-- easily integrade those parsers into the CLI option parser of your
-- own executable program.
module NetSpider.CLI
       ( module NetSpider.CLI.Spider,
         module NetSpider.CLI.Snapshot
       ) where

import Options.Applicative (Parser)

import NetSpider.CLI.Spider
import NetSpider.CLI.Snapshot
