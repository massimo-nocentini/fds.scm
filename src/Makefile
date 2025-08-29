

test:
	cd test && csi -s queue.scm
	cd test && csi -s sbral.scm

install:
	chicken-install -sudo

format:
	scheme-indent -T 2 < fds.queue.scm > fds.queue.scm.tmp && mv fds.queue.scm.tmp fds.queue.scm
	scheme-indent -T 2 < fds.sbral.scm > fds.sbral.scm.tmp && mv fds.sbral.scm.tmp fds.sbral.scm
	scheme-indent -T 2 < test/queue.scm > test/queue.scm.tmp && mv test/queue.scm.tmp test/queue.scm
	scheme-indent -T 2 < test/sbral.scm > test/sbral.scm.tmp && mv test/sbral.scm.tmp test/sbral.scm
