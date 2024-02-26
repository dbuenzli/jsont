# Testing

    b0 test
    
# Testing the codec with Nicolas Seriot's test suite

    b0 -- download-seriot-suite
    b0 test


# Benchmarking 

   hyperfine 'jq -c . tmp/parcels.json'
   hyperfine 'jsontrip tmp/parcels.json'
   hyperfine "$(b0 --path -- jsont) fmt --no-locs -fminify tmp/parcels.json"
    
