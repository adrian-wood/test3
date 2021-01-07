import unittest
import tempfile
import filecmp
from collections import defaultdict
import RetrievalTable as RT
import DataAccessLog as DA


class Test_Retrieval_Table(unittest.TestCase):
    def setUp(self):
        self.rt = RT.RetrievalTable("test_data/retrieval_table")

    def test_count_datatypes(self):
        self.assertEqual(len(self.rt.list_datatypes()), 11)

    def test_list_datatypes(self):
        self.assertEqual(
            sorted(self.rt.list_datatypes()),
            sorted(
                [
                    "AATSR",
                    "JASON2",
                    "LIDAR",
                    "LNDSYN",
                    "METARS",
                    "SHPSYN",
                    "TAFS",
                    "SUNPHOTO",
                    "TEMP",
                    "VA_LIDAR",
                    "STNMAS",
                ]
            ),
        )

    def test_count_datasets(self):
        self.assertEqual(self.rt.dataset_count("AATSR"), 1)
        self.assertEqual(self.rt.dataset_count("JASON2"), 1)
        self.assertEqual(self.rt.dataset_count("LIDAR"), 1)
        self.assertEqual(self.rt.dataset_count("LNDSYN"), 10)
        self.assertEqual(self.rt.dataset_count("METARS"), 8)
        self.assertEqual(self.rt.dataset_count("SHPSYN"), 13)
        self.assertEqual(self.rt.dataset_count("TAFS"), 2)
        self.assertEqual(self.rt.dataset_count("SUNPHOTO"), 1)
        self.assertEqual(self.rt.dataset_count("TEMP"), 12)
        self.assertEqual(self.rt.dataset_count("VA_LIDAR"), 1)

    def test_output(self):
        # test the output file is identical in format
        temp = tempfile.NamedTemporaryFile()
        try:
            self.rt.write_RT(temp.name)
            self.assertTrue(
                filecmp.cmp(temp.name, "test_data/retrieval_table", shallow=False)
            )
        finally:
            # Automatically cleans up the file
            temp.close()


class Test_Data_Access_Log(unittest.TestCase):
    def setUp(self):
        with open("test_data/data_access.log", errors="ignore") as f:
            self.da = DA.DataAccessLog(f)

    def test_retrieval_count(self):
        self.assertEqual(self.da.retrieval_count, 1000)

    def test_count_by_datatype(self):
        self.assertEqual(self.da.count_by_datatype["SNOW"], 1)
        self.assertEqual(self.da.count_by_datatype["ESACSR"], 2)
        self.assertEqual(self.da.count_by_datatype["MWRI"], 2)
        self.assertEqual(self.da.count_by_datatype["WOW"], 2)
        self.assertEqual(self.da.count_by_datatype["AMSR2"], 3)
        self.assertEqual(self.da.count_by_datatype["SREW"], 48)
        self.assertEqual(self.da.count_by_datatype["RADRRATE"], 50)
        self.assertEqual(self.da.count_by_datatype["BUOY"], 55)
        self.assertEqual(self.da.count_by_datatype["LNDSYN"], 83)
        self.assertEqual(self.da.count_by_datatype["SHPSYN"], 86)

    def test_count_by_userid(self):
        self.assertEqual(self.da.count_by_userid["uktrials"], 556)
        self.assertEqual(self.da.count_by_userid["radarnet"], 12)
        self.assertEqual(self.da.count_by_userid["freb"], 143)
        self.assertEqual(self.da.count_by_userid["frwm"], 77)
        self.assertEqual(self.da.count_by_userid["jwaller"], 56)
        self.assertEqual(self.da.count_by_userid["gltrials"], 3)
        self.assertEqual(self.da.count_by_userid["cmao"], 3)
        self.assertEqual(self.da.count_by_userid["jroberts"], 1)
        self.assertEqual(self.da.count_by_userid["ahorsema"], 2)
        self.assertEqual(self.da.count_by_userid["chthomas"], 2)

    def test_count_by_contact(self):
        self.assertEqual(self.da.count_by_contact["OPS"], 870)
        self.assertEqual(self.da.count_by_contact["adam.maycock@metoffice.gov.uk"], 24)
        self.assertEqual(self.da.count_by_contact["mi-ba291_mi-ba187"], 12)
        self.assertEqual(self.da.count_by_contact["srbest"], 8)
        self.assertEqual(self.da.count_by_contact["ed.stone@metoffice.gov.uk"], 6)
        self.assertEqual(self.da.count_by_contact["mi-ba076"], 1)
        self.assertEqual(
            self.da.count_by_contact["standalone.apps@metoffice.gov.uk"], 1
        )
        self.assertEqual(self.da.count_by_contact["mi-au425"], 2)
        self.assertEqual(self.da.count_by_contact["mi-aw600"], 2)
        self.assertEqual(self.da.count_by_contact["mode-s-realtime"], 4)

    def test_userid_contact(self):
        uktrials = defaultdict(int)
        uktrials["OPS"] = 543
        uktrials["mi-ba003"] = 3
        uktrials["mi-ba002"] = 3
        uktrials["mi-az998"] = 3
        uktrials["mi-ba340"] = 1
        uktrials["mi-ba329"] = 1
        uktrials["mi-ba077"] = 1
        uktrials["mi-ba076"] = 1
        self.assertEqual(self.da.userid_contact["uktrials"], uktrials)

    def test_contact_userid(self):
        ops = defaultdict(int)
        ops["uktrials"] = 543
        ops["freb"] = 143
        ops["frwm"] = 77
        ops["jwaller"] = 56
        ops["hadci"] = 14
        ops["frlh"] = 11
        ops["mjardak"] = 5
        ops["ppdev"] = 4
        ops["frbg"] = 4
        ops["gltrials"] = 3
        ops["cmao"] = 3
        ops["frnb"] = 2
        ops["frdv"] = 2
        ops["chthomas"] = 2
        ops["jroberts"] = 1
        self.assertEqual(self.da.contact_userid["OPS"], ops)


if __name__ == "__main__":
    unittest.main()
