# -*- coding: utf-8 -*-

import re
import socket


class Matcher(object):
    """Matches incoming lines according to the internal protocol and calls back
    on matched tags and versions.
    """

    def __init__(self):
        """Initializes the matcher.
        """
        self.__callbacks = {}
        self.__regexp = re.compile(r"\{(?P<tag>\w+),(?P<vsn>\d+),\{(?P<Ms>\d+),(?P<s>\d+),(?P<us>\d+)\},(?P<content>.+)\}\.")

    def add_callback(self, tag, version, callback):
        """Adds function to internal list of callbacks to be called then
        message with specified tag and version is received.
        
        Arguments:
        - `tag`: Message tag, i.e. 'air_temp', 'commentary', etc.
        - `version`: Message version.
        - `callback`: Function to be called back with timestamp and message
        content as parameters.
        """
        self.__callbacks.setdefault(tag, {}).setdefault(version, []).append(callback)

    def match(self, line):
        """Parses the line and calls handlers if there is any for the matched
        message.
        
        Arguments:
        - `line`: Line received from the server.
        """
        result = self.__regexp.match(line.strip())
        if result:
            tag = result.group("tag")
            vsn = int(result.group("vsn"))
            timestamp = Matcher.__convert_timestamp(result.group("Ms"), result.group("s"), result.group("us"))
            content = result.group("content")
            for callback in self.__callbacks.get(tag, {}).get(vsn, []):
                callback(timestamp, content)

    @staticmethod
    def __convert_timestamp(megaseconds, seconds, microseconds):
        return float(megaseconds) * 1.0E6 + float(seconds) + float(microseconds) * 1.0E-6


class Client(object):
    """Simple client which combines message receiving with matching and
    calling back.
    """
    
    def __init__(self, ):
        """Initializes the client.
        """
        object.__init__(self)
        self.__socket = None
        self.__stream = None
        self.matcher = Matcher()

    def close(self):
        """Closes connection to the server.
        """
        self.__socket.close()
        self.__socket = None
        self.__stream = None

    def connect(self, endpoint):
        """Connects to the server on specified endpoint.
        
        Arguments:
        - `endpoint`: Pair (tuple of two elements) of hostname and port.
        """
        self.__socket = socket.create_connection(endpoint)
        self.__stream = self.__socket.makefile()

    def run(self):
        """Receives messages from server line by line, parses them and calls
        back appropriate handlers if there is any. Blocks if there is no data
        from server right away.
        """
        for line in self.__stream:
            self.matcher.match(line)


if __name__ == "__main__":
    def on_air_temp(timestamp, content):
        print "Timestamp %f, air temp %f °C" % (timestamp, float(content))
    client = Client()
    client.connect(endpoint=("localhost", 8642))
    client.matcher.add_callback(tag="air_temp", version=0, callback=on_air_temp)
    try:
        client.run()
    except KeyboardInterrupt:
        pass
