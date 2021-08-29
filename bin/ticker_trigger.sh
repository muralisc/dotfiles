#!/bin/bash

# 13 March Price
START_PRICE=61283
TARGET_PRICE=$(bc <<< "scale=2; $START_PRICE * (1 - 0.15)")
PRICE_NOW=$(
curl -s "https://query1.finance.yahoo.com/v7/finance/quote?lang=en-US&region=US&corsDomain=finance.yahoo.com&symbols=BTC-USD" \
  | jq '.quoteResponse.result[0].regularMarketPrice'
  )
MESSAGE=$(echo $PRICE_NOW [$(bc <<< "scale=2; $PRICE_NOW/$START_PRICE" )])
curl -s \
  --form-string "token=ahu1ti348q8ezgnegkahksrzb8broj" \
  --form-string "user=uh3grj3d1b9qfujpkjb8yo53iotrm4" \
  --form-string "message=$MESSAGE" \
  https://api.pushover.net/1/messages.json
