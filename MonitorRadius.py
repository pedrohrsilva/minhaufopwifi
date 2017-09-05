import time
import psycopg2
from datetime import datetime
from datetime import timedelta

class Session(object):
    sessions = []
    def __init__(self, row):
        super(Session, self).__init__()
        self.isNew = row[0] not in Session.sessions
        if self.isNew:
          Session.sessions.append(row[0])
        self.row = row


    def insert_session(self):
        fields = "(radacctid, username, nasportid, acctstarttime, 
                    acctstoptime, acctsessiontime, acctinputoctets, 
                    acctoutputoctets, callingstationid, 
                    acctterminatecause, framedipaddress, timeop)"
        if self.isNew:
            query = "insert into sessions "+fields+" values 
                    ("+",".join(["'"+str(x)+"'" for x in self.row])+");"
        else:
            update = ",".join(["'"+str(x)+"'" for x in [ self.row[4], 
                     self.row[5], self.row[6], self.row[7], 
                     self.row[9], self.row[11] ]])
            query = "update sessions set (acctstoptime, 
                    acctsessiontime, acctinputoctets, acctoutputoctets, 
                    acctterminatecause, timeop)"
            query = query + " = (" + update + ") where 
                    radacctid = " + str(self.row[0])
        return query.replace("'None'", "NULL")
        

    def insert_traffic(self):
        values = ",".join(["'"+str(x)+"'" 
                 for x in [ self.row[0], self.row[5]
                 , self.row[6], self.row[7], self.row[11] ]])
        query = "insert into traffic (radacctid, acctsessiontime
                  , acctinputoctets, acctoutputoctets, timeop) values "
        query = query + "("+values+");"
        return query.replace("'None'", "NULL")

    @classmethod
    def get_sessions_count(cls):
        return len(cls.sessions)

start = datetime.now()
while True:
  try:
    connRadius = psycopg2.connect("dbname='radius' user=USER host=RADIUSHOST  password=RADIUSPASS")
  except:
    print "Nao foi possivel se conectar ao banco de dados RADIUS"
    exit()
  try:
    connLocal = psycopg2.connect("dbname='sessions' user=LOCALUSER 
    host='localhost' password=LOCALPASS")
  except:
    print "Nao foi possivel se conectar ao banco de dados Local"
    exit()
  curRadius = connRadius.cursor()
  curRadius.execute("""select radacctid, username, nasportid, acctstarttime, 
                    acctstoptime, acctsessiontime, acctinputoctets, 
                    acctoutputoctets, callingstationid, acctterminatecause,
                    framedipaddress, now() from radacct where acctstoptime 
                    is null OR acctstoptime >= now()::timestamp - 
                    interval '5 minutes'""")
  rows = curRadius.fetchall()
  countSessions = Session.get_sessions_count()
  for row in rows:
    session = Session(row)
    curLocal = connLocal.cursor()
    curLocal.execute(session.insert_session())
    curLocal.execute(session.insert_traffic())
  countSessions = Session.get_sessions_count() - countSessions
  if len(rows) > 0:
    print(str(rows[0][11])+" "+str(len(rows))+" current and 
         "+str(countSessions)+" new sessions")
    curLocal.execute("""insert into new_sessions 
           (timeop, started, current) values (%s,%s,%s)""",
           (rows[0][11],countSessions,len(rows)))
  else:
    print("0 current and 0 new sessions")
  connLocal.commit()
  connLocal.close()
  connRadius.close()
  start = start + timedelta(minutes=5)
  delay = (start - datetime.now()).total_seconds()
  time.sleep(delay)