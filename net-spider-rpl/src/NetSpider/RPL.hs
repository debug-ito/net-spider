{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: NetSpider.RPL
-- Description: NetSpider data model and utility for RPL networks
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This module defines NetSpider data model and utility for RPL
-- networks.
--
-- RPL ( [IPv6 Routing Protocol for Low-Power and Lossy Networks, RFC 6550](https://tools.ietf.org/html/rfc6550) )
-- is a routing protocol for small wireless devices. Each node
-- independently exchanges routing information with its neighbors over
-- low-power wireless channels. Using those information, the nodes
-- form one or more network topologies called DODAG
-- (Destination-Oriented Directed Asynclic Graph) in a distributed
-- manner.
--
-- The module "NetSpider.RPL.DIO" defines NetSpider data model about
-- DIO (DODAG Information Object). DIO is a control packet of RPL that
-- is necessary to form upward routes. 'FoundNodeDIO' type defines
-- DIO-specific information such as \"rank\". You can input
-- 'FoundNodeDIO's to NetSpider database to construct the upward
-- network.
--
-- The "NetSpider.RPL.DAO" defines another data model
-- 'FoundNodeDAO'. DAO (Destination Advertisement Object) is a control
-- packet of RPL that is necessary to form downward
-- routes. 'FoundNodeDAO' type defines DAO-specific information. You
-- can input 'FoundNodeDAO' to NetSpider database to construct the
-- downward network.
--
-- Because network topologies maintained in 'FoundNodeDIO' and
-- 'FoundNodeDAO' can be different, you have to use different node IDs
-- for them. The module "NetSpider.RPL.FindingID" exports 'FindingID'
-- that distingushes those two types of topologies. The DIO and DAO
-- topologies are obtained from the NetSpider database as different
-- Snapshot graphs.
--
-- The two types of Snapshot graphs can be combined together by
-- upgrading their internal data. "NetSpider.RPL.Combined" module
-- provides functions to combine those graphs.
--
-- The module "NetSpider.RPL.ContikiNG" (not re-exported by this
-- module) is a utility module to make 'FoundNodeDIO's and
-- 'FoundNodeDAO's. It defines utility functions to read and parse log
-- files generated by Contiki-NG applications.
-- [Contiki-NG](http://contiki-ng.org/) is a tiny operating system for
-- wireless devices that supports RPL.
module NetSpider.RPL
       ( module NetSpider.RPL.FindingID,
         module NetSpider.RPL.DIO,
         module NetSpider.RPL.DAO,
         module NetSpider.RPL.Combined
       ) where

import NetSpider.RPL.FindingID
import NetSpider.RPL.DIO
import NetSpider.RPL.DAO
import NetSpider.RPL.Combined
