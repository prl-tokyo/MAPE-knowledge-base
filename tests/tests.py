import hug
from mapekb import api
import unittest
import json

class TestMonitoringEndpoint(unittest.TestCase):

    def test_put_returns_ok(self):
        self.assertEqual("200 OK",
                         hug.test.put(api,
                                      'v1/monitor',
                                      {'resource': "ec2:0001",
                                       'status': "OK"}
                         ).status
        )

    def test_put_returns_json(self):
        self.assertEqual("application/json",
                         hug.test.put(api,
                                      'v1/monitor',
                                      {'resource': "ec2:0001",
                                       'status': "OK"}
                         ).content_type
        )

    def test_put_missing_resource_returns_error(self):
        self.assertEqual({'errors': {'resource': 'Required parameter not supplied'}},
                         hug.test.put(api,
                                      'v1/monitor',
                                      {'status': "OK"}
                         ).data
        )

    def test_put_missing_status_returns_error(self):
        self.assertEqual({'errors': {'status': 'Required parameter not supplied'}},
                         hug.test.put(api,
                                      'v1/monitor',
                                      {'resource': "ec2:0001"}
                         ).data
        )
