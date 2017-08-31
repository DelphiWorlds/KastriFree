#!/bin/bash

# This script builds the iOS and Mac openSSL libraries
# Download openssl http://www.openssl.org/source/ and place the tarball next to this script

# Credits:
# https://github.com/st3fan/ios-openssl
# https://github.com/x2on/OpenSSL-for-iPhone/blob/master/build-libssl.sh
# Peter Steinberger, PSPDFKit GmbH, @steipete.
# Doron Adler, GlideTalk, @Norod78
# Dave Nottage, RADSoft, @DelphiWorlds

# Updated to work with Xcode 8.3.3 and iOS 10
# This script is significantly modified from the original (as per the links above)
# It generates dylibs for i386 and x86_64 architectures, and can be used on iOS simulator only
# The selected version of the OpenSSL source must be expanded first, and the Makefile.shared file modified to suit

set -e

###################################
# 		 OpenSSL Version
###################################
OPENSSL_VERSION="openssl-1.1.0f"
###################################

###################################
# 		 SDK Version
###################################
IOS_SDK_VERSION=$(xcodebuild -version -sdk iphoneos | grep SDKVersion | cut -f2 -d ':' | tr -d '[[:space:]]')
###################################

################################################
# 		 Minimum iOS deployment target version
################################################
MIN_IOS_VERSION="8.0"

################################################
# 		 Minimum OS X deployment target version
################################################
MIN_OSX_VERSION="10.10"

echo "----------------------------------------"
echo "OpenSSL version: ${OPENSSL_VERSION}"
echo "iOS SDK version: ${IOS_SDK_VERSION}"
echo "iOS deployment target: ${MIN_IOS_VERSION}"
echo "OS X deployment target: ${MIN_OSX_VERSION}"
echo "----------------------------------------"
echo " "

DEVELOPER=`xcode-select -print-path`
buildMac()
{
	ARCH=$1
	echo "Start Building ${OPENSSL_VERSION} for ${ARCH}"
	TARGET="darwin-i386-cc"
	if [[ $ARCH == "x86_64" ]]; then
		TARGET="darwin64-x86_64-cc"
	fi
	
	export CC="${BUILD_TOOLS}/usr/bin/clang -mmacosx-version-min=${MIN_OSX_VERSION}"
	pushd . > /dev/null
	cd "${OPENSSL_VERSION}"
	echo "Configure"
	./Configure ${TARGET} --openssldir="/tmp/${OPENSSL_VERSION}-${ARCH}" --prefix="/tmp/${OPENSSL_VERSION}-${ARCH}" shared &> "/tmp/${OPENSSL_VERSION}-${ARCH}.log"
	make >> "/tmp/${OPENSSL_VERSION}-${ARCH}.log" 2>&1
	echo "make install"
	make install >> "/tmp/${OPENSSL_VERSION}-${ARCH}.log" 2>&1
	echo "make clean"
	make clean >> "/tmp/${OPENSSL_VERSION}-${ARCH}.log" 2>&1
	popd > /dev/null
	
	echo "Done Building ${OPENSSL_VERSION} for ${ARCH}"
}
buildIOS()
{
	ARCH=$1
	
	pushd . > /dev/null
	cd "${OPENSSL_VERSION}"
  
	if [[ "${ARCH}" == "i386" || "${ARCH}" == "x86_64" ]]; then
		PLATFORM="iPhoneSimulator"
	else
		PLATFORM="iPhoneOS"
		sed -ie "s!static volatile sig_atomic_t intr_signal;!static volatile intr_signal;!" "crypto/ui/ui_openssl.c"
	fi
  
	export $PLATFORM
	export CROSS_TOP="${DEVELOPER}/Platforms/${PLATFORM}.platform/Developer"
	export CROSS_SDK="${PLATFORM}${IOS_SDK_VERSION}.sdk"
	export BUILD_TOOLS="${DEVELOPER}"
	export CC="${BUILD_TOOLS}/usr/bin/gcc -mios-version-min=${MIN_IOS_VERSION} -arch ${ARCH}"
	
	echo "Start Building ${OPENSSL_VERSION} for ${PLATFORM} ${IOS_SDK_VERSION} ${ARCH}"
  echo "Configure"
	if [[ "${ARCH}" == "x86_64" ]]; then
		./Configure darwin64-x86_64-cc --openssldir="/tmp/${OPENSSL_VERSION}-iOS-${ARCH}" --prefix="/tmp/${OPENSSL_VERSION}-iOS-${ARCH}" shared &> "/tmp/${OPENSSL_VERSION}-iOS-${ARCH}.log"
	else
		./Configure iphoneos-cross --openssldir="/tmp/${OPENSSL_VERSION}-iOS-${ARCH}" --prefix="/tmp/${OPENSSL_VERSION}-iOS-${ARCH}" shared &> "/tmp/${OPENSSL_VERSION}-iOS-${ARCH}.log"
	fi
	# add -isysroot to CC=
	sed -ie "s!^CFLAG=!CFLAG=-isysroot ${CROSS_TOP}/SDKs/${CROSS_SDK} -mios-version-min=${MIN_IOS_VERSION} !" "Makefile"
	# replace gcc with clang, since sysroot no longer works with gcc since Xcode 8.1
	sed -ie 's/\/usr\/bin\/gcc/\/Toolchains\/XcodeDefault.xctoolchain\/usr\/bin\/clang/g' Makefile
  echo "make"
	make >> "/tmp/${OPENSSL_VERSION}-iOS-${ARCH}.log" 2>&1
	echo "make install"
	make install >> "/tmp/${OPENSSL_VERSION}-iOS-${ARCH}.log" 2>&1
	echo "make clean"
	make clean  >> "/tmp/${OPENSSL_VERSION}-iOS-${ARCH}.log" 2>&1
	popd > /dev/null
	
	echo "Done Building ${OPENSSL_VERSION} for ${ARCH}"
}
echo "Cleaning up"
rm -rf include/openssl/* lib/*
rm -rf /tmp/${OPENSSL_VERSION}-*
rm -rf "/tmp/${OPENSSL_VERSION}-*.log"
#skipping cleanup of the openssl source, because Makefile.shared is modified beforehand, manually
#rm -rf ${OPENSSL_VERSION}
mkdir -p lib/iOS
mkdir -p include/openssl/
if [ ! -e ${OPENSSL_VERSION}.tar.gz ]; then
	echo "Downloading ${OPENSSL_VERSION}.tar.gz"
	curl -O https://www.openssl.org/source/${OPENSSL_VERSION}.tar.gz
else
	echo "Using ${OPENSSL_VERSION}.tar.gz"
fi
#echo "Unpacking openssl"
#skipping expansion, Makefile.shared is modified beforehand, manually
#tar xfz "${OPENSSL_VERSION}.tar.gz"
buildMac "i386"
buildMac "x86_64"
echo "Copying headers"
cp /tmp/${OPENSSL_VERSION}-i386/include/openssl/* include/openssl/
buildIOS "x86_64"
buildIOS "i386"
echo "Building iOS libraries"
lipo \
	"/tmp/${OPENSSL_VERSION}-iOS-i386/lib/libcrypto.1.1.dylib" \
	"/tmp/${OPENSSL_VERSION}-iOS-x86_64/lib/libcrypto.1.1.dylib" \
	-create -output lib/iOS/libcrypto.1.1.dylib
lipo \
	"/tmp/${OPENSSL_VERSION}-iOS-i386/lib/libssl.1.1.dylib" \
	"/tmp/${OPENSSL_VERSION}-iOS-x86_64/lib/libssl.1.1.dylib" \
	-create -output lib/iOS/libssl.1.1.dylib
echo "Cleaning up"
rm -rf /tmp/${OPENSSL_VERSION}-*
#skipping cleanup of the openssl source, because Makefile.shared is modified beforehand, manually
#rm -rf ${OPENSSL_VERSION}
echo "Done"
