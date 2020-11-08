run_learn:
	sbt -DMode=learn run

run_explore:
	sbt -DMode=explore run

clean_all_knowledge:
	echo "implement me please"

kill_agent:
	curl http://localhost:8080/killagent

