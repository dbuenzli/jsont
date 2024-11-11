# Testing

    b0 test
    
# Testing the codec with Nicolas Seriot's test suite

    b0 -- download-seriot-suite
    b0 test


# Benchmarking 

## Decode only 

    hyperfine 'json_xs -t none < tmp/parcels.json'
    hyperfine 'jsontrip -dec tmp/parcels.json'
    hyperfine "$(b0 --path -- jsont) fmt -d tmp/parcels.json"
    hyperfine "$(b0 --path -- geojson) -d tmp/parcels.json"

## Decode and minify 

    hyperfine 'json_xs -t json < tmp/parcels.json'
    hyperfine 'jq -c . tmp/parcels.json'
    hyperfine 'ydump -std -c tmp/parcels.json'
    hyperfine 'jsontrip tmp/parcels.json'
    hyperfine "$(b0 --path -- jsont) fmt -fminify tmp/parcels.json"
    hyperfine "$(b0 --path -- geojson) tmp/parcels.json"


