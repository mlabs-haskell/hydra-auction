module HydraAuction.Onchain.Types.BidderInfo (
  BidderInfo (..),
  BidderInfo'Error (..),
  validateBidderInfo,
) where

import PlutusTx.Prelude

import PlutusLedgerApi.V1 (PubKeyHash)
import PlutusTx qualified

-- import HydraAuction.Onchain.Lib.Error (eCode, err)
import HydraAuction.Error.Types.BidderInfo (BidderInfo'Error (..))

data BidderInfo = BidderInfo
  { bi'BidderPkh :: !PubKeyHash
  -- ^ Bidder's pubkey hash, which can spend this bidder deposit
  -- to buy the auction lot if a bid placed by bi'BidderVk wins
  -- or reclaim this bid deposit if someone else's bid wins.
  , bi'BidderVk :: !BuiltinByteString
  -- ^ Bidder's verification, which can authorize bids that allow
  -- the seller at'SellerPkh to claim this bidder deposit
  -- if the bid placed by bi'BidderVk won but the auction lot
  -- wasn't purchased by the deadline.
  }

instance Eq BidderInfo where
  (BidderInfo x1 x2)
    == (BidderInfo y1 y2) =
      x1 == y1
        && x2 == y2

PlutusTx.unstableMakeIsData ''BidderInfo

-- -------------------------------------------------------------------------
-- Validation
-- -------------------------------------------------------------------------

validateBidderInfo ::
  BidderInfo ->
  Bool
validateBidderInfo _ =
  --
  -- The bidder's hashed payment verification key corresponds
  -- to the bidder's payment verification key.
  -- (bi'BidderPkh == PubKeyHash (blake2b_224 bi'BidderVk))
  --   `err` $(eCode BidderInfo'Error'BidderVkPkhMismatch)
  True
--
{-# INLINEABLE validateBidderInfo #-}
