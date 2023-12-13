module Plutus.Cardano.Api.Codec.Time (
  -- Offchain -> Onchain
  toPlutusUTCTime,
  toPlutusUTCTimeMilli,
  -- Onchain -> Offchain
  fromPlutusUTCTimeMilli,
  fromPlutusUTCTime,
) where

import Data.Function ((&))

import Data.Time.Clock qualified as Cardano.Api.Time

import PlutusLedgerApi.V1.Time qualified as PV1.Time

import Plutus.Cardano.Api.Types.Time.Millisecond qualified as Codec.Types.Time

-- -------------------------------------------------------------------------
-- Millisecond time
-- -------------------------------------------------------------------------
toPlutusUTCTimeMilli ::
  Codec.Types.Time.UTCTimeMilli ->
  PV1.Time.POSIXTime
toPlutusUTCTimeMilli cTimeUTCMilli =
  cTimeUTCMilli
    & Codec.Types.Time.utcTimeMilliToPOSIXMilli
    & Codec.Types.Time.posixTimeMilliToInteger
    & PV1.Time.POSIXTime

fromPlutusUTCTimeMilli ::
  PV1.Time.POSIXTime ->
  Codec.Types.Time.UTCTimeMilli
fromPlutusUTCTimeMilli (PV1.Time.POSIXTime pTimePOSIXInteger) =
  pTimePOSIXInteger
    & Codec.Types.Time.integerToPOSIXTimeMilli
    & Codec.Types.Time.posixMilliToUTCMilli

-- -------------------------------------------------------------------------
-- Picosecond time (Cardano API) and Millisecond time (Plutus)
-- -------------------------------------------------------------------------
-- This is a lossy conversion from Cardano API picosecond time
-- to Plutus millisecond time.
toPlutusUTCTime ::
  Cardano.Api.Time.UTCTime ->
  PV1.Time.POSIXTime
toPlutusUTCTime cTimeUTC =
  cTimeUTC
    & Codec.Types.Time.restrictUTCTimeMilli
    & Codec.Types.Time.utcTimeMilliToPOSIXMilli
    & Codec.Types.Time.posixTimeMilliToInteger
    & PV1.Time.POSIXTime

-- This function requires a reference UTC time
-- to provide the sub-millisecond information
-- that is missing in Plutus' millisecond time
-- but is needed in Cardano API's picosecond time.
--
-- Typically, this reference time should be set to the
-- system start time of the Cardano node to which
-- queries and transactions are being submitted,
-- so that slot boundaries are properly aligned.
fromPlutusUTCTime ::
  PV1.Time.POSIXTime ->
  Cardano.Api.Time.UTCTime ->
  Cardano.Api.Time.UTCTime
fromPlutusUTCTime (PV1.Time.POSIXTime pTimePOSIXInteger) referenceTimeUTC =
  pTimePOSIXInteger
    & Codec.Types.Time.integerToPOSIXTimeMilli
    & Codec.Types.Time.posixMilliToUTCMilli
    & Codec.Types.Time.restoreUTCTime referenceTimeUTC
