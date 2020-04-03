let global_defs = ./global-defs as Text
let virtual_server = ./virtual-server

in \(ips : List IP) -> ''
${global_defs}

${virtual_server (IP.IPv4 "127.0.0.2") 443 ips}
''
