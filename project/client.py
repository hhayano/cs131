#!/usr/local/bin/python3

import asyncio
import sys

PORTS = {
        "Goloman": 12229,
        "Hands": 12230,
        "Holiday": 12231,
        "Welsh": 12232,
        "Wilkes": 12233
        }

class ClientProtocol(asyncio.Protocol):
    def __init__(self, loop):
        self.loop = loop

    def connection_made(self, transport):
        self.transport = transport
        message = input("Please enter your message: ")
        transport.write(message.encode())
        print('Data sent: {!r}'.format(message))

    def data_received(self, data):
        print('Data received: {}'.format(data.decode()))
        message = input("Please enter your message: ")
        self.transport.write(message.encode())
        print('Data sent: {}'.format(message))

    def connection_lost(self, exc):
        self.loop.stop()

def main():
    loop = asyncio.get_event_loop()
    
    server_name = input("Which server do you want to connect to?\n(Goloman, Hands, Holiday, Welsh, Wilkes):")
    print("will connect to server port {}".format(PORTS.get(server_name, 9000)))
    
    coro = loop.create_connection(lambda: ClientProtocol(loop),
                                          '127.0.0.1', PORTS.get(server_name, 9000))
    loop.run_until_complete(coro)
    try:
        loop.run_forever()
    except KeyboardInterrupt:
        pass
    loop.close()

if __name__ == "__main__":
    main()
