import hug
from mapekb import api
import unittest
import json

class TestMonitoringEndpoint(unittest.TestCase):

    def test_put_returns_ok(self):
        self.assertEqual("200 OK",
                         hug.test.put(api,
                                      'v1/monitor',
                                      {'status': {}}
                         ).status
        )
