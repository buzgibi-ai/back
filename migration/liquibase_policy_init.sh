#!/bin/sh
# ============LICENSE_START====================================================
#  Copyright (C) 2021. Nordix Foundation. All rights reserved.
# =============================================================================
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0
# ============LICENSE_END======================================================

/liquibase/liquibase \
    --driver=org.postgresql.Driver \
    --url=jdbc:postgresql://db:5432/buzgibi \
    --changeLogFile=changelog/changelog.xml \
    --username=sonny \
    --password=3190d261d186aeead3a8deec202737c7775af5c8d455a9e5ba958c48b5fd3f59 \
    --log-level info \
    update