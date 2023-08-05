#COMBILE RML SPECIFICATION TO PRO LOG
java -jar ./rmlcompiler/rml-compiler.jar --input ./specification/pendulum_stabilize.rml --output ./monitor/pendulum_stabilize.pl
#RUN RML WEB SERVER
sh ./online_monitor.sh ./monitor/pendulum_stabilize.pl 8080