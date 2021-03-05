pipeline {
    agent any
    options {
        buildDiscarder(logRotator(numToKeepStr: '3'))
    }
    environment {
        IMAGE = 'glpgmedicalmonitoring'
        NS = 'glpgmedicalmonitoring'
        REG = '196229073436.dkr.ecr.eu-west-1.amazonaws.com'
        TAG = sh(returnStdout: true, script: "echo $BRANCH_NAME | sed -e 's/[A-Z]/\\L&/g' -e 's/[^a-z0-9._-]/./g'").trim()
        DOCKER_BUILDKIT = '1'
    }
    stages {
        stage('Build Image') {
            agent {
                kubernetes {
                    yaml '''
                    apiVersion: v1
                    kind: Pod
                    spec:
                      containers:
                      - name: dind
                        image: 196229073436.dkr.ecr.eu-west-1.amazonaws.com/oa-infrastructure/dind
                        securityContext:
                          privileged: true'''
                    defaultContainer 'dind'
                }
            }
            steps {
				copyArtifacts filter: '*.tar.gz', fingerprintArtifacts: true, projectName: 'git/glpgStyle/master', selector: lastSuccessful()
                copyArtifacts filter: '*.tar.gz', fingerprintArtifacts: true, projectName: 'git/GLPGUtilityFct/master', selector: lastSuccessful()   
                copyArtifacts filter: '*.tar.gz', fingerprintArtifacts: true, projectName: 'git/GLPGPatientProfiles/master', selector: lastSuccessful()
                copyArtifacts filter: '*.tar.gz', fingerprintArtifacts: true, projectName: 'git/GLPGInTextSummaryTable/master', selector: lastSuccessful()   
                withOARegistry {
                    sh "docker build --build-arg BUILDKIT_INLINE_CACHE=1 --cache-from ${env.REG}/${env.NS}/${env.IMAGE}:${env.TAG} --cache-from ${env.REG}/${env.NS}/${env.IMAGE}:master -t ${env.NS}/${env.IMAGE}:${env.TAG} -f Dockerfile ."
                }
                ecrPush "${env.REG}", "${env.NS}/${env.IMAGE}", "${env.TAG}", '', 'eu-west-1'
            }
        }
        stage('Packages') {
            agent {
                kubernetes {
                    yaml """
                    apiVersion: v1
                    kind: Pod
                    spec:
                      containers:
                      - name: r
                        image: ${env.REG}/${env.NS}/${env.IMAGE}:${env.TAG}
                        command: 
                        - cat
                        tty: true
                        imagePullPolicy: Always"""
                    defaultContainer 'r'
                }
            }
            stages {
                stage('medicalMonitoring') {
                    stages {
                        stage('Roxygen') {
                            steps {
                                sh 'R -q -e \'roxygen2::roxygenize("package/medicalMonitoring")\''
                            }
                        }
                        stage('Build') {
                            steps {
                                sh 'R CMD build package/medicalMonitoring'
                            }
                        }
                        stage('Check (no tests)') {
                            steps {
                                sh 'ls medicalMonitoring_*.tar.gz && R CMD check medicalMonitoring_*.tar.gz --no-manual --no-tests'
                            }
                        }
                        stage('Install') {
                            steps {
                                  sh 'R -q -e \'install.packages(list.files(".", "medicalMonitoring_.*.tar.gz"), repos = NULL) \''
                            }
                        }
                        stage('Test and coverage') {
                            steps {
                              sh '''
                                R -q -e \'
                                pc <- covr::package_coverage("package/medicalMonitoring", type = "none", code = "testthat::test_package(\\"medicalMonitoring\\", reporter = testthat::JunitReporter$new(file = file.path(Sys.getenv(\\"WORKSPACE\\"), \\"results.xml\\")))")
                                covr::report(x = pc, file = paste0("testCoverage-", attr(pc, "package")$package, "-", attr(pc, "package")$version, ".html"))
                                covr::to_cobertura(pc)
                                \'
                                zip -r testCoverage.zip lib/ testCoverage*.html
                               '''
                            }
                            post {
                                always {
                                    junit 'results.xml'
                                    cobertura autoUpdateHealth: false, autoUpdateStability: false, coberturaReportFile: 'cobertura.xml', conditionalCoverageTargets: '70, 0, 0', failUnhealthy: false, failUnstable: false, lineCoverageTargets: '80, 0, 0', maxNumberOfBuilds: 0, methodCoverageTargets: '80, 0, 0', onlyStable: false, sourceEncoding: 'ASCII', zoomCoverageChart: false
                                }
                            }
                        }
                    }
                }
                stage('Archive artifacts') {
                    steps {
                        archiveArtifacts artifacts: '*.tar.gz, *.pdf, **/00check.log, testCoverage.zip', fingerprint: true
                    }
                }
            }
        }
    }
}

